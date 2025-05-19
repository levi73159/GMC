const std = @import("std");
const TypeVal = @import("Token.zig").TypeValue;
const Pos = @import("DebugPos.zig");

const Self = @This();

value: TypeVal,
generic_type: ?*const Self,

pos: ?Pos = null,

pub fn init(value: TypeVal, generic_type: ?*const Self) Self {
    return Self{ .value = value, .generic_type = generic_type };
}

pub inline fn any() Self {
    return Self{ .value = .any, .generic_type = null };
}

pub fn getGenericInfo(self: Self) ?TypeVal.GenericInfo {
    return self.value.getGenericInfo();
}

pub fn checkGenericError(self: Self) error{ ExpectedGenericType, NotAGeneric }!void {
    if (self.getGenericInfo()) |info| {
        if (info.default == null and self.generic_type == null) return error.ExpectedGenericType;
    } else {
        if (self.generic_type != null) return error.NotAGeneric;
    }
}

pub fn isBasicType(self: Self) bool {
    if (self.generic_type != null) return false;

    return switch (self.value) {
        .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .int, .bool, .char, .str => true,
        else => false,
    };
}

pub fn size(self: Self) ?u32 {
    return switch (self.value) {
        .i8, .u8 => 1,
        .i16, .u16 => 2,
        .i32, .u32 => 4,
        .i64, .u64 => 8,
        .int => 4,
        .float => 8, // float (f64)
        .bool => 1,
        .char => 1,
        .str => 8, // strings are pointers (8 bytes)
        .list, .imlist => 8, // list is a pointer (8 bytes)
        .any, .anyenum => null,
        .void, .type => 0, // type is a type that doesn't have a size cause it comptime known
    };
}

pub fn equal(self: Self, other: Self) bool {
    if (self.value != other.value) return false;
    if (self.generic_type == null and other.generic_type == null) return true; // value == value and generics == generics
    if (self.generic_type) |gen| {
        if (other.generic_type) |other_gen| {
            return gen.equal(other_gen.*);
        }
    }
    return false;
}

pub fn format(
    self: Self,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: std.io.AnyWriter,
) !void {
    try writer.print("{s}", .{@tagName(self.value)});
    if (self.generic_type) |gen| {
        try writer.print("<{}>", .{gen});
    }
}
