const std = @import("std");
const rt = @import("runtime.zig");
const types = @import("types.zig");
const Type = @import("Type.zig");

const Self = @This();

pub const SymbolValue = union(enum) {
    u8: u8,
    u16: u16,
    u32: u32,
    u64: u64,

    i8: i8,
    i16: i16,
    i32: i32,
    i64: i64,

    float: f64,
    bool: bool,
    void: void,

    string: types.String,
    char: u8, // char is another name for u8 but treated like a character instead of a number
    func: types.Function,
    list: *types.List, // any thing that is mutable like a list will be a pointer to the type

    pub fn deinit(self: SymbolValue) void {
        switch (self) {
            .string => |s| {
                s.deinit();
            },
            .list => |l| {
                l.deinit();
            },
            else => {},
        }
    }

    pub fn clone(self: SymbolValue) SymbolValue {
        return switch (self) {
            .string => |s| SymbolValue{ .string = s.clone() },
            .list => |l| SymbolValue{ .list = l.clone() },
            else => self,
        };
    }

    pub fn ref(self: SymbolValue) SymbolValue {
        return switch (self) {
            .string => |s| SymbolValue{ .string = s.ref() },
            .list => |l| SymbolValue{ .list = l.ref() },
            else => self,
        };
    }

    pub fn setGenericType(self: SymbolValue, ty: Type) !void {
        switch (self) {
            .list => |l| try l.setGenericType(ty),
            else => return error.NotGeneric,
        }
    }
};
pub const SymbolType = std.meta.Tag(SymbolValue);

pub const Symbol = struct {
    is_const: bool,
    value: SymbolValue,

    pub fn deinit(self: Symbol) void {
        self.value.deinit();
    }

    pub fn clone(self: Symbol) Symbol {
        return Symbol{ .is_const = self.is_const, .value = self.value.clone() };
    }

    pub fn ref(self: Symbol) Symbol {
        return Symbol{ .is_const = self.is_const, .value = self.value.ref() };
    }
};

parent: ?*Self = null,
table: std.StringHashMap(Symbol),
allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) Self {
    return Self{ .table = std.StringHashMap(Symbol).init(allocator), .allocator = allocator };
}

pub fn deinit(self: *Self) void {
    var it = self.table.iterator();
    while (it.next()) |entry| {
        entry.value_ptr.value.deinit();
        self.allocator.free(entry.key_ptr.*);
    }
    self.table.deinit();
}

pub fn add(self: *Self, name: []const u8, symbol: Symbol) !void {
    if (self.table.contains(name)) {
        return error.SymbolAlreadyExists;
    }
    try self.table.putNoClobber(try self.allocator.dupe(u8, name), symbol);
}

pub fn set(self: *Self, name: []const u8, value: SymbolValue) !void {
    const symbol = self.getPtr(name) orelse return error.SymbolDoesNotExist;
    if (symbol.is_const) return error.SymbolIsImmutable;
    // check if the type is the same as value
    const ty = std.meta.activeTag(symbol.value);
    const val = std.meta.activeTag(value);
    if (ty != val) return error.InvalidTypes;
    symbol.value.deinit(); // deinit the old value
    symbol.value = value; // init the new value with the clone
}

pub fn get(self: Self, name: []const u8) ?Symbol {
    if (self.table.get(name)) |symbol| return symbol;
    if (self.parent) |parent| return parent.get(name);
    return null;
}

pub fn getPtr(self: *Self, name: []const u8) ?*Symbol {
    if (self.table.getPtr(name)) |symbol| return symbol;
    if (self.parent) |parent| return parent.getPtr(name);
    return null;
}
