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
