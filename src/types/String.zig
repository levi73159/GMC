const std = @import("std");
const SymbolTable = @import("../SymbolTable.zig");
const TypeVal = @import("../Token.zig").TypeValue;
const rt = @import("../runtime.zig");

const Value = rt.Value;
const Result = rt.Result;

const Self = @This();

pub const Inner = struct {
    refs: u32,
};
value: []const u8,
mem_type: union(enum) {
    heap: *Inner,
    stack: void,
},
allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator, value: []const u8, is_heap: bool) !Self {
    if (is_heap) {
        const inner = try allocator.create(Inner);
        inner.* = Inner{ .refs = 1 };
        return Self{ .value = value, .mem_type = .{ .heap = inner }, .allocator = allocator };
    }
    return Self{ .value = value, .mem_type = .stack, .allocator = allocator };
}

pub fn fromChar(char: u8, allocator: std.mem.Allocator) Self {
    return Self{ .value = &[_]u8{char}, .mem_type = .stack, .allocator = allocator };
}

pub fn fromConcatChars(c1: u8, c2: u8, allocator: std.mem.Allocator) Self {
    return Self{ .value = &[_]u8{ c1, c2 }, .mem_type = .stack, .allocator = allocator };
}

pub fn deinit(self: Self) void {
    switch (self.mem_type) {
        .heap => |h| {
            h.refs -= 1;
            if (h.refs == 0) {
                self.allocator.free(self.value);
                self.allocator.destroy(h);
            }
        },
        .stack => {},
    }
}

pub fn clone(self: Self) Self {
    return self.ref(); // since strings are immutable we don't need to copy
}

pub fn ref(self: Self) Self {
    return Self{ .value = self.value, .mem_type = switch (self.mem_type) {
        .heap => |heap| blk: {
            const inner = heap;
            inner.refs += 1;
            break :blk .{ .heap = inner };
        },
        .stack => .stack,
    }, .allocator = self.allocator };
}

pub fn concat(self: Self, other: Self) !Self {
    const new_value = try std.mem.concat(self.allocator, u8, &[_][]const u8{ self.value, other.value });
    const inner = try self.allocator.create(Self.Inner);
    inner.* = Self.Inner{ .refs = 1 };

    defer self.deinit();
    defer other.deinit();
    const s = Self{
        .value = new_value,
        .mem_type = .{ .heap = inner },
        .allocator = self.allocator,
    };
    return s;
}

pub fn concatRaw(self: Self, other: []const u8) !Self {
    const new_value = try std.mem.concat(self.allocator, u8, &[_][]const u8{ self.value, other });
    const inner = try self.allocator.create(Self.Inner);
    inner.* = Self.Inner{ .refs = 1 };

    defer self.deinit();
    return Self{
        .value = new_value,
        .mem_type = .{ .heap = inner },
        .allocator = self.allocator,
    };
}

/// returns error.NegativeRepeat if times < 0
pub fn repeat(self: Self, times: i65) !Self {
    if (times < 0) return error.NegativeRepeat;
    if (times == 0) return Self{ .value = "", .mem_type = .stack, .allocator = self.allocator };

    const new_len = self.value.len * @as(usize, @intCast(times));
    const new_value = try self.allocator.alloc(u8, new_len);
    errdefer self.allocator.free(new_value);

    var i: usize = 0;
    while (i < new_len) : (i += 1) {
        new_value[i] = self.value[i % self.value.len];
    }

    const inner = try self.allocator.create(Self.Inner);
    inner.* = Self.Inner{ .refs = 1 };

    self.deinit();
    return Self{
        .value = new_value,
        .mem_type = .{ .heap = inner },
        .allocator = self.allocator,
    };
}

/// returns error.NegativeRepeat if times < 0
pub fn initRepeat(allocator: std.mem.Allocator, char: u8, times: i65) !Self {
    if (times < 0) return error.NegativeRepeat;
    if (times == 0) return Self{ .value = "", .mem_type = .stack, .allocator = allocator };

    const new_len = @as(usize, @intCast(times));
    const copy_value = try allocator.alloc(u8, new_len);
    var i: usize = 0;
    while (i < new_len) : (i += 1) {
        copy_value[i] = char;
    }

    const inner = try allocator.create(Self.Inner);
    inner.* = Self.Inner{ .refs = 1 };

    return Self{
        .value = copy_value,
        .mem_type = .{ .heap = inner },
        .allocator = allocator,
    };
}

pub fn equal(self: Self, other: Self) bool {
    if (self.value.len != other.value.len) return false;
    return std.mem.eql(u8, self.value, other.value);
}

pub fn equalChar(self: Self, other: u8) bool {
    if (self.value.len != 1) return false;
    return self.value[0] == other;
}

pub fn index(self: Self, i: Value) Value {
    defer self.deinit();
    const iv = rt.castToIndex(i, self.value.len) catch return Value.err("IndexError", "Can't cast to index", null);
    if (iv < 0 or iv >= self.value.len) return Value.err("IndexError", "Index out of range", null);
    return Value{ .char = self.value[iv] };
}
