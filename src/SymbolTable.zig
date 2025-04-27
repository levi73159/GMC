const std = @import("std");

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

    f32: f32,
    f64: f64,

    bool: bool,
    void: void,
};
pub const SymbolType = std.meta.Tag(SymbolValue);

pub const Symbol = struct {
    is_const: bool,
    value: SymbolValue,
};

parent: ?*Self = null,
table: std.StringHashMap(Symbol),
allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) Self {
    return Self{ .table = std.StringHashMap(Symbol).init(allocator), .allocator = allocator };
}

pub fn deinit(self: *Self) void {
    var key_it = self.table.keyIterator();
    while (key_it.next()) |key| self.allocator.free(key.*);
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
    symbol.value = value;
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
