const std = @import("std");
const SymbolTable = @import("../SymbolTable.zig");
const TypeVal = @import("../Token.zig").TypeValue;
const Type = @import("../Type.zig");

const rt = @import("../runtime.zig");

const Value = rt.Value;
const Result = rt.Result;

const Self = @This();
const GROW_FACTOR = 2;

const funcs = @import("List.funcs.zig");

allocator: std.mem.Allocator,
items: []SymbolTable.Symbol = &.{},
capacity: u64 = 0,
refs: u32 = 1,
immutable: bool = false,

item_type: Type = Type.any(),

pub fn init(allocator: std.mem.Allocator, immutable: bool) *Self {
    const list = allocator.create(Self) catch unreachable;
    list.* = Self{ .allocator = allocator, .immutable = immutable };
    return list;
}

pub fn recursiveMutablityCurrent(self: *Self) *Self {
    return self.recursiveMutablity(self.immutable);
}

pub fn recursiveMutablity(self: *Self, immutable: bool) *Self {
    self.immutable = immutable;
    for (self.items) |*item| {
        item.is_const = immutable;
        switch (item.value) {
            .list => |l| _ = l.recursiveMutablity(immutable),
            else => {},
        }
    }
    return self;
}

pub fn initMutable(allocator: std.mem.Allocator) *Self {
    const list = allocator.create(Self) catch unreachable;
    list.* = Self{ .allocator = allocator, .immutable = false };
    return list;
}

pub fn initImmutable(allocator: std.mem.Allocator) *Self {
    const list = allocator.create(Self) catch unreachable;
    list.* = Self{ .allocator = allocator, .immutable = true };
    return list;
}

pub fn fromItems(allocator: std.mem.Allocator, items: []const Value) *Self {
    const list = Self.initMutable(allocator);
    list.appendSlice(items) catch unreachable;
    return list;
}

pub fn fromItemsImmutable(allocator: std.mem.Allocator, items: []const Value) *Self {
    const list = Self.initImmutable(allocator);
    list.appendSlice(items) catch unreachable;
    return list;
}

pub fn fromSymbols(allocator: std.mem.Allocator, items: []const SymbolTable.Symbol) *Self {
    const list = Self.initMutable(allocator);
    list.appendSliceSymbols(items) catch unreachable;
    return list;
}

pub fn fromSymbolsImmutable(allocator: std.mem.Allocator, items: []const SymbolTable.Symbol) *Self {
    const list = Self.initImmutable(allocator);
    list.appendSliceSymbols(items) catch unreachable;
    return list;
}

pub fn deinit(self: *Self) void {
    self.refs -|= 1;
    for (self.items) |item| item.value.deinit();

    if (self.refs == 0) {
        self.clearAndFree();
        self.allocator.destroy(self);
    }
}

// returns the cloned list (does not decrese or increase the ref count)
pub fn clone(self: *Self) *Self {
    const cloned_items = self.allocator.alloc(SymbolTable.Symbol, self.items.len) catch unreachable;
    defer self.allocator.free(cloned_items);
    for (cloned_items, self.items) |*new, old| new.* = old.clone();
    const list = Self.fromSymbols(self.allocator, cloned_items); // immutablity doesn't matter cause it is handled by the caller
    return list;
}

// returns itself and increases the ref count by one
pub fn ref(self: *Self) *Self {
    self.refs += 1;
    for (self.items) |item| _ = item.ref();
    return self;
}

fn clearAndFree(self: *Self) void {
    if (self.capacity == 0) return;
    self.allocator.free(self.items.ptr[0..self.capacity]);
    self.items = &.{};
    self.capacity = 0;
}

pub const clear = clearAndFree;

pub fn append(self: *Self, value: Value) !void {
    if (self.immutable) return error.ImmutableList;
    const symbol_value = try rt.castToSymbolValue(self.allocator, value, self.item_type.value);
    if (self.item_type.generic_type) |gen_type| try symbol_value.setGenericType(gen_type.*);
    return self.appendSymbol(symbol_value);
}

pub fn appendSymbol(self: *Self, value: SymbolTable.SymbolValue) !void {
    if (self.immutable) return error.ImmutableList;
    try self.needGrow();
    self.items.ptr[self.items.len] = SymbolTable.Symbol{ .value = value, .is_const = self.immutable };
    self.items.len += 1;
}

pub fn appendSlice(self: *Self, values: []const Value) !void {
    if (self.immutable) return error.ImmutableList;
    if (self.capacity < values.len) try self.resize(@intCast(self.capacity + values.len)); // grow if need
    for (values, self.items.ptr[self.items.len..self.capacity]) |value, *item| {
        const symbol_value = try rt.castToSymbolValue(self.allocator, value, self.item_type);
        item.* = SymbolTable.Symbol{ .value = symbol_value, .is_const = self.immutable };
    }
    self.items.len += values.len;
}

/// NOTE: does not check for the same type, have to be done by the caller
pub fn appendSliceSymbols(self: *Self, values: []const SymbolTable.Symbol) !void {
    if (self.immutable) return error.ImmutableList;
    if (self.capacity < values.len) try self.resize(@intCast(self.capacity + values.len)); // grow if need
    @memcpy(self.items.ptr[self.items.len..][0..values.len], values);
    self.items.len += values.len;
}

pub fn pop(self: *Self) !?SymbolTable.Symbol {
    if (self.immutable) return error.ImmutableList;
    if (self.items.len == 0) return null;
    self.items.len -= 1;
    const item = self.items.ptr[self.items.len];
    defer item.value.deinit();
    if (self.items.len == 0) self.clearAndFree(); // dealloc memory if empty
    return item;
}

// check if we need to grow if so then we grow
fn needGrow(self: *Self) !void {
    if (self.capacity <= self.items.len + 1) try self.grow();
}

// for immutable lists resize is a no-op
pub fn resize(self: *Self, len: usize) !void {
    if (self.immutable) return;
    if (self.items.len == len) return;
    std.debug.assert(len > self.items.len);
    const old_len = self.items.len;
    self.items = try self.allocator.realloc(self.items.ptr[0..self.capacity], len);
    self.capacity = len;
    self.items.len = old_len;
}

fn grow(self: *Self) !void {
    const old_len = self.items.len;
    const new_cap = if (self.capacity == 0) 8 else self.capacity * GROW_FACTOR;
    self.items = try self.allocator.realloc(self.items.ptr[0..self.capacity], new_cap);
    self.capacity = new_cap;
    self.items.len = old_len;
}

pub fn equal(self: *const Self, other: *const Self) Value {
    if (self.items.len != other.items.len) return rt.Value{ .boolean = false };
    for (self.items, other.items) |a, b| {
        const a_rtvalue = rt.castToValueNoRef(a.value);
        const b_rtvalue = rt.castToValueNoRef(b.value);

        const boolean_value = a_rtvalue.equal(b_rtvalue);
        if (rt.checkRuntimeError(boolean_value, null)) |err| return err.value;
        if (!boolean_value.boolean) return rt.Value{ .boolean = false };
    }
    return rt.Value{ .boolean = true };
}

pub fn setImmutable(self: *Self, immutable: bool) *Self {
    self.immutable = immutable;
    return self;
}

pub fn setGenericType(self: *Self, generic_type: Type) anyerror!void {
    self.item_type = generic_type;
    for (self.items) |*item| {
        item.value = try rt.castToSymbolValue(self.allocator, rt.castToValueNoRef(item.value), generic_type.value);
        if (generic_type.getGenericInfo()) |info| {
            if (generic_type.generic_type) |gen_type| {
                try item.value.setGenericType(gen_type.*);
            } else if (info.default) |default| {
                try item.value.setGenericType(Type.init(default, null));
            } else {
                return error.ExpectedGenericType;
            }
        } else {
            if (generic_type.generic_type) |_| return error.NotGeneric;
        }
        item.is_const = self.immutable;
    }
}

pub fn repeat(self: *Self, times: i65) !*Self {
    if (times < 0) return error.NegativeRepeat;
    if (times == 0) return Self.initMutable(self.allocator);

    const times_usize: usize = @intCast(times);
    const new = self.clone(); // copy the list, aka times 1
    for (0..times_usize - 1) |_| {
        try new.appendSliceSymbols(self.items);
    }
    return new;
}

pub fn index(self: *Self, i: Value) Value {
    defer self.deinit();
    const iv = rt.castToIndex(i, self.items.len) catch return Value.err("IndexError", "Can't cast to index", null);
    if (iv < 0 or iv >= self.items.len) return Value.err("IndexError", "Index out of range", null);
    _ = self.items[iv].value.ref();
    return Value{ .symbol = .{
        .ptr = &self.items[iv],
        .type = self.item_type,
    } };
}

pub fn field(self: *Self, name: []const u8) Value {
    defer self.deinit();
    if (std.mem.eql(u8, name, "length")) {
        return Value{ .integer = @intCast(@as(isize, @intCast(self.items.len))) };
    } else {
        const decls = @typeInfo(funcs).@"struct".decls;
        inline for (decls) |decl| {
            if (std.mem.eql(u8, name, decl.name)) {
                const func = @field(funcs, decl.name);
                return Value{ .func = .{
                    .bultin = .{
                        .name = decl.name,
                        .func = func,
                        .is_method = true,
                    },
                } };
            }
        }
        return Value.errPrint(self.allocator, "AttributeError", "'List' object has no attribute '{s}'", .{name}, null);
    }
}
