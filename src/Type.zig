const std = @import("std");
const TypeVal = @import("Token.zig").TypeValue;
const Pos = @import("DebugPos.zig");
const Symbol = @import("SymbolTable.zig").Symbol;
const types = @import("types.zig");

const Self = @This();

var global_uuid: u64 = 8;
pub const DefineType = struct {
    name: []const u8,
    ty: *const Symbol, // the actual type null when parser defined
    global_uuid: u64, // for quick comparisons
};

pub const TypeValue = union(enum) {
    builtin: TypeVal,
    defined: *const DefineType,

    pub fn eqlBuiltin(self: TypeValue, other: TypeVal) bool {
        return switch (self) {
            .builtin => |bty| bty == other,
            else => false,
        };
    }

    pub fn eqlDefined(self: TypeValue, other: *const DefineType) bool {
        return switch (self) {
            .defined => |dty| dty.global_uuid == other.global_uuid,
            else => false,
        };
    }

    pub fn eql(self: TypeValue, other: Self) bool {
        if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;
        return switch (self) {
            .builtin => |bty| bty == other.builtin,
            .defined => |dty| dty.global_uuid == other.defined.global_uuid,
        };
    }
};

var defined_types: ?std.ArrayList(DefineType) = null;

value: TypeValue,
generic_type: ?*const Self,

pos: ?Pos = null,

pub fn register(allocator: std.mem.Allocator, name: []const u8) u64 {
    if (defined_types == null) defined_types = std.ArrayList(DefineType).init(allocator);

    for (defined_types.?.items) |t| {
        if (std.mem.eql(u8, t.name, name)) return t.global_uuid;
    }

    const guid = global_uuid;
    global_uuid += 1;
    defined_types.?.append(DefineType{ .name = name, .ty = undefined, .global_uuid = guid }) catch unreachable;
    return guid;
}

pub fn setType(allocator: std.mem.Allocator, name: []const u8, ty: *const Symbol) u64 {
    if (defined_types == null) defined_types = std.ArrayList(DefineType).init(allocator);

    for (defined_types.?.items) |*t| {
        if (std.mem.eql(u8, t.name, name)) {
            t.* = .{ .name = name, .ty = ty, .global_uuid = t.global_uuid };
            return t.global_uuid;
        }
    }

    const guid = global_uuid;
    global_uuid += 1;
    defined_types.?.append(.{ .name = name, .ty = ty, .global_uuid = guid }) catch unreachable;
    return guid;
}

pub fn deinit() void {
    if (defined_types == null) return;
    defined_types.?.deinit();
    defined_types = null;
}

pub fn getType(name: []const u8) ?TypeValue {
    for (defined_types.?.items) |*t| {
        if (std.mem.eql(u8, t.name, name)) return TypeValue{ .defined = t };
    } else return null;
}

fn abstractTypeInfo(ty: *const Symbol) types.TypeInfo {
    return switch (ty.value) {
        .@"enum" => |e| e.asTypeInfo(),
        else => unreachable,
    };
}

pub fn getTypeInfo(uuid: u64) ?types.TypeInfo {
    if (defined_types == null) return null;

    for (defined_types.?.items) |t| {
        if (t.global_uuid == uuid) return abstractTypeInfo(t.ty);
    } else return null;
}

pub fn init(value: TypeVal, generic_type: ?*const Self) Self {
    return Self{ .value = .{ .builtin = value }, .generic_type = generic_type };
}

pub inline fn any() Self {
    return Self{ .value = .{ .builtin = .any }, .generic_type = null };
}

pub fn getGenericInfo(self: Self) ?TypeVal.GenericInfo {
    if (self.value == .defined) return null;
    return self.value.builtin.getGenericInfo();
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
    if (self.value == .defined) return false;

    return switch (self.value.builtin) {
        .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .int, .bool, .char, .str => true,
        else => false,
    };
}

pub fn size(self: Self) ?u32 {
    if (self.value == .defined) return null;
    return switch (self.value.builtin) {
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
    if (std.meta.activeTag(self.value) != std.meta.activeTag(other.value)) return false;
    switch (self.value) {
        .builtin => |bty| if (bty != other.value.builtin) return false,
        .defined => |dty| if (dty.global_uuid != other.value.defined.global_uuid) return false,
    }

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
