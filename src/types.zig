const std = @import("std");
const Pos = @import("DebugPos.zig");
const tree = @import("tree.zig");

const Intrepreter = @import("Interpreter.zig");
const Params = tree.FuncParam;
const TypeVal = @import("Token.zig").TypeValue;
const SymbolTable = @import("SymbolTable.zig");

const rt = @import("runtime.zig");
const Value = rt.Value;
const Result = rt.Result;

pub const Error = struct {
    msg: []const u8,
    extra: ?[]const u8,
    pos: ?Pos,

    extra_allocated: ?std.mem.Allocator = null, // null = false (default aka stack), value = allocated
    pub fn deinit(self: Error) void {
        if (self.extra_allocated) |allocator| {
            if (self.extra) |extra| allocator.free(extra);
        }
    }

    pub fn equal(_: Error, _: Error) bool {
        return false;
    }
};

pub const String = struct {
    pub const Inner = struct {
        refs: u32,
    };
    value: []const u8,
    mem_type: union(enum) {
        heap: *Inner,
        stack: void,
    },
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, value: []const u8, is_heap: bool) !String {
        if (is_heap) {
            const inner = try allocator.create(Inner);
            inner.* = Inner{ .refs = 1 };
            return String{ .value = value, .mem_type = .{ .heap = inner }, .allocator = allocator };
        }
        return String{ .value = value, .mem_type = .stack, .allocator = allocator };
    }

    pub fn fromChar(char: u8, allocator: std.mem.Allocator) String {
        return String{ .value = &[_]u8{char}, .mem_type = .stack, .allocator = allocator };
    }

    pub fn fromConcatChars(c1: u8, c2: u8, allocator: std.mem.Allocator) String {
        return String{ .value = &[_]u8{ c1, c2 }, .mem_type = .stack, .allocator = allocator };
    }

    pub fn deinit(self: String) void {
        switch (self.mem_type) {
            .heap => |h| {
                h.refs -|= 1;
                if (h.refs == 0) {
                    self.allocator.free(self.value);
                    self.allocator.destroy(h);
                }
            },
            .stack => {},
        }
    }

    pub fn clone(self: String) String {
        return self.ref(); // since strings are immutable we don't need to copy
    }

    pub fn ref(self: String) String {
        return String{ .value = self.value, .mem_type = switch (self.mem_type) {
            .heap => |heap| blk: {
                const inner = heap;
                inner.refs += 1;
                break :blk .{ .heap = inner };
            },
            .stack => .stack,
        }, .allocator = self.allocator };
    }

    pub fn concat(self: String, other: String) !String {
        const new_value = try std.mem.concat(self.allocator, u8, &[_][]const u8{ self.value, other.value });
        const inner = try self.allocator.create(String.Inner);
        inner.* = String.Inner{ .refs = 1 };

        self.deinit();
        other.deinit();
        return String{
            .value = new_value,
            .mem_type = .{ .heap = inner },
            .allocator = self.allocator,
        };
    }

    pub fn concatRaw(self: String, other: []const u8) !String {
        const new_value = try std.mem.concat(self.allocator, u8, &[_][]const u8{ self.value, other });
        const inner = try self.allocator.create(String.Inner);
        inner.* = String.Inner{ .refs = 1 };

        self.deinit();
        return String{
            .value = new_value,
            .mem_type = .{ .heap = inner },
            .allocator = self.allocator,
        };
    }

    /// returns error.NegativeRepeat if times < 0
    pub fn repeat(self: String, times: i65) !String {
        if (times < 0) return error.NegativeRepeat;
        if (times == 0) return String{ .value = "", .mem_type = .stack, .allocator = self.allocator };

        const new_len = self.value.len * @as(usize, @intCast(times));
        const new_value = try self.allocator.alloc(u8, new_len);
        errdefer self.allocator.free(new_value);

        var i: usize = 0;
        while (i < new_len) : (i += 1) {
            new_value[i] = self.value[i % self.value.len];
        }

        const inner = try self.allocator.create(String.Inner);
        inner.* = String.Inner{ .refs = 1 };

        self.deinit();
        return String{
            .value = new_value,
            .mem_type = .{ .heap = inner },
            .allocator = self.allocator,
        };
    }

    /// returns error.NegativeRepeat if times < 0
    pub fn initRepeat(allocator: std.mem.Allocator, char: u8, times: i65) !String {
        if (times < 0) return error.NegativeRepeat;
        if (times == 0) return String{ .value = "", .mem_type = .stack, .allocator = allocator };

        const new_len = @as(usize, @intCast(times));
        const copy_value = try allocator.alloc(u8, new_len);
        var i: usize = 0;
        while (i < new_len) : (i += 1) {
            copy_value[i] = char;
        }

        const inner = try allocator.create(String.Inner);
        inner.* = String.Inner{ .refs = 1 };

        return String{
            .value = copy_value,
            .mem_type = .{ .heap = inner },
            .allocator = allocator,
        };
    }

    pub fn equal(self: String, other: String) bool {
        if (self.value.len != other.value.len) return false;
        return std.mem.eql(u8, self.value, other.value);
    }

    pub fn equalChar(self: String, other: u8) bool {
        if (self.value.len != 1) return false;
        return self.value[0] == other;
    }

    pub fn index(self: String, i: Value) Value {
        defer self.deinit();
        const iv = rt.castToIndex(i, self.value.len) catch return Value.err("IndexError", "Can't cast to index", null);
        if (iv < 0 or iv >= self.value.len) return Value.err("IndexError", "Index out of range", null);
        return Value{ .char = self.value[iv] };
    }
};

pub const BaseFunction = struct {
    name: []const u8,
    params: []const tree.FuncParam,
    body: *const tree.Node, // root node to execute
    return_type: TypeVal,
    parent_scope: ?*SymbolTable,

    // debug pos, might be usefull but not needed
    name_pos: ?Pos = null,
    return_type_pos: ?Pos = null,

    pub fn call(self: BaseFunction, args: []const Value, base: Intrepreter) Result {
        var symbols = SymbolTable.init(base.allocator);
        defer symbols.deinit();

        if (self.params.len != args.len) {
            const msg = std.fmt.allocPrint(base.allocator, "Expected {d} arguments got {d}", .{ self.params.len, args.len }) catch unreachable;
            return Result.errHeap(base.allocator, "Invalid number of arguments", msg, null);
        }

        for (self.params, args) |param, arg| {
            const symbol_value = rt.castToSymbolValue(base.allocator, arg, param.type.value.type) catch {
                const msg2 = std.fmt.allocPrint(base.allocator, "Can't cast {s} to {s}", .{ @tagName(arg), @tagName(param.type.value.type) }) catch unreachable;
                return Result.errHeap(base.allocator, "Invalid cast", msg2, null);
            };

            symbols.add(param.name.lexeme, SymbolTable.Symbol{ .value = symbol_value, .is_const = true }) catch @panic("out of memory");
        }
        // setting scope heres mean that if there is a param x and a varaible x outside of the function it will be shadowed by the Params
        // if we don't wan't that and want it to cause an error we can put this line before we add the params
        symbols.parent = self.parent_scope;

        const scope = base.newScope(&symbols, base.allocator);
        const ret = scope.evalNode(self.body);
        if (rt.checkRuntimeErrorOrSignal(ret, self.body)) |err| return err;
        return ret;
    }

    // not the best but get the job done
    pub fn staticCheck(self: BaseFunction, base: Intrepreter) ?Result {
        var arena = std.heap.ArenaAllocator.init(base.allocator);
        defer arena.deinit();

        const allocator = arena.allocator();

        var symbols = SymbolTable.init(allocator);
        defer symbols.deinit();

        for (self.params) |param| {
            const symbol_value = rt.castToSymbolValue(base.allocator, Value.none, param.type.value.type) catch {
                const msg2 = std.fmt.allocPrint(base.allocator, "Can't cast {s} to {s}", .{ @tagName(Value.none), @tagName(param.type.value.type) }) catch unreachable;
                return Result.errHeap(base.allocator, "Invalid cast", msg2, null);
            };

            symbols.add(param.name.lexeme, SymbolTable.Symbol{ .value = symbol_value, .is_const = true }) catch @panic("out of memory");
        }

        symbols.parent = self.parent_scope;

        var scope = base.newScope(&symbols, allocator);
        scope.static = true;
        const ret = scope.evalNode(self.body);
        if (rt.checkRuntimeErrorOrSignal(ret, self.body)) |sigOrErr| switch (sigOrErr) {
            .signal => |signal| switch (signal) {
                .@"return" => |v| {
                    _ = rt.castToSymbolValue(allocator, v, self.return_type) catch {
                        const msg = std.fmt.allocPrint(base.allocator, "Function return an expected type, Expected {s} got {s}", .{ @tagName(self.return_type), @tagName(ret.value) }) catch unreachable;
                        return Result.errHeap(base.allocator, "Invalid return type", msg, self.return_type_pos);
                    };
                    // we can cast the value
                    return null;
                },
                else => return sigOrErr,
            },
            else => return sigOrErr,
        };

        if (self.return_type != .void) {
            const msg = std.fmt.allocPrint(base.allocator, "Function return an expected type, Expected {s} got void", .{@tagName(self.return_type)}) catch unreachable;
            return Result.errHeap(base.allocator, "Invalid return type", msg, self.return_type_pos);
        }

        return null;
    }
};

pub const BultinFunction = struct {
    name: []const u8,
    func: *const fn (args: []const Value, base: Intrepreter) Result,

    pub fn call(self: BultinFunction, args: []const Value, base: Intrepreter) Result {
        return self.func(args, base);
    }
};

pub const Function = union(enum) {
    base: BaseFunction,
    bultin: BultinFunction,

    pub fn getName(self: Function) []const u8 {
        return switch (self) {
            .base => |f| f.name,
            .bultin => |f| f.name,
        };
    }

    pub fn call(self: Function, args: []const Value, base: Intrepreter) Result {
        return switch (self) {
            .base => |f| f.call(args, base),
            .bultin => |f| f.call(args, base),
        };
    }

    pub fn equal(self: Function, other: Function) bool {
        const self_name = self.getName();
        const other_name = other.getName();
        return std.mem.eql(u8, self_name, other_name);
    }
};

const GROW_FACTOR = 2;
pub const List = struct {
    allocator: std.mem.Allocator,
    items: []Value = &.{},
    capacity: u64 = 0,
    refs: u32 = 1,
    immutable: bool = false,

    const Self = @This();

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
        for (self.items) |item| switch (item) {
            .list => |l| _ = l.recursiveMutablity(immutable),
            else => {},
        };

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

    pub fn deinit(self: *Self) void {
        if (self.capacity == 0) return;
        self.refs -= 1;

        for (self.items) |item| item.deinit();

        if (self.refs == 0) {
            self.clearAndFree();
            self.allocator.destroy(self);
        }
    }

    // returns the cloned list (does not decrese or increase the ref count)
    pub fn clone(self: *Self) *Self {
        const cloned_items = self.allocator.alloc(Value, self.items.len) catch unreachable;
        defer self.allocator.free(cloned_items);
        for (cloned_items, self.items) |*new, old| new.* = old.clone();
        const list = Self.fromItems(self.allocator, cloned_items);
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

    pub fn clear(self: *Self) !void {
        if (self.immutable) return error.ImmutableList;
        if (self.capacity == 0) return;
        self.items.len = 0;
    }

    pub fn append(self: *Self, value: Value) !void {
        if (self.immutable) return error.ImmutableList;
        try self.needGrow();
        self.items.ptr[self.items.len] = value;
        self.items.len += 1;
    }

    pub fn appendSlice(self: *Self, values: []const Value) !void {
        if (self.immutable) return error.ImmutableList;
        if (self.capacity < values.len) try self.resize(@intCast(self.capacity + values.len)); // grow if need
        @memcpy(self.items.ptr[self.items.len .. self.items.len + values.len], values);
        self.items.len += values.len;
    }

    pub fn pop(self: *Self) !?Value {
        if (self.immutable) return error.ImmutableList;
        if (self.items.len == 0) return null;
        self.items.len -= 1;
        if (self.items.len == 0) self.clearAndFree(); // dealloc memory if empty
        return self.items[self.items.len];
    }

    // check if we need to grow if so then we grow
    fn needGrow(self: *Self) !void {
        if (self.capacity <= self.items.len + 1) try self.grow();
    }

    // for immutable lists resize is a no-op
    pub fn resize(self: *Self, len: usize) !void {
        if (self.immutable) return;
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

    pub fn equal(self: *const Self, other: *const Self) bool {
        if (self.items.len != other.items.len) return false;
        for (self.items, other.items) |a, b| {
            if (!a.equalB(b)) return false;
        }
        return true;
    }

    pub fn index(self: *Self, i: Value) Value {
        defer self.deinit();
        const iv = rt.castToIndex(i, self.items.len) catch return Value.err("IndexError", "Can't cast to index", null);
        if (iv < 0 or iv >= self.items.len) return Value.err("IndexError", "Index out of range", null);
        _ = self.items[iv].ref();
        if (self.immutable) {
            return self.items[iv];
        } else {
            return Value{ .ptr = &self.items[iv] };
        }
    }

    pub fn setImmutable(self: *Self, immutable: bool) *Self {
        self.immutable = immutable;
        return self;
    }
};
