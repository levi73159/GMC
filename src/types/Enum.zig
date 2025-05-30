const std = @import("std");
const SymbolTable = @import("../SymbolTable.zig");
const TypeVal = @import("../Token.zig").TypeValue;
const Type = @import("../Type.zig");

const rt = @import("../runtime.zig");
const ty = @import("../types.zig");

const Value = rt.Value;
const Result = rt.Result;

var enum_uuid: u64 = 0; // the next uuid to be used only used for enums

pub const Field = struct {
    name: []const u8,
    value: SymbolTable.SymbolValue, // the actual value
    backing_value: u64, // for quick comparisons
};

// Enum Instance aka doing EnumType.name will return instance with said field
pub const Instance = struct {
    field: *const Field,
    type_uuid: u64, // not from the same enum
    global_uuid: u64, // not from the same type
    strict: bool = true,

    pub fn equal(self: Instance, other: Instance) bool {
        if (self.type_uuid != other.type_uuid) return false; // not
        return self.field.backing_value == other.field.backing_value;
    }
};

const Self = @This();

enum_name: []const u8,
allocator: std.mem.Allocator,
fields: []const Field,
uuid: u64 = 0, // unique identifier for each enum instance
global_uuid: u64 = 0,

tagged: Type = Type.init(.i32, null),

pub fn init(allocator: std.mem.Allocator, name: []const u8, fields: []const Field, tagged: Type) Self {
    const uuid = enum_uuid;
    enum_uuid += 1;
    return Self{ .allocator = allocator, .enum_name = name, .fields = fields, .uuid = uuid, .tagged = tagged };
}

pub fn deinit(self: Self) void {
    for (self.fields) |f| f.value.deinit();
    self.allocator.free(self.fields);
}

pub fn asTypeInfo(self: Self) ty.TypeInfo {
    return ty.TypeInfo{
        .define = .{
            .type_name = self.enum_name,
            .base_name = "Enum",
            .size = self.tagged.size() orelse @panic("Enum size is null"),
            .is_builtin = false,
        },
        .type_uuid = self.uuid,
        .global_uuid = self.global_uuid,
        .is_generic = false,
    };
}

pub fn field(self: Self, name: []const u8) Value {
    for (self.fields) |*f| {
        if (std.mem.eql(u8, f.name, name)) {
            return Value{
                .enum_instance = Instance{
                    .field = f,
                    .type_uuid = self.uuid,
                    .global_uuid = self.global_uuid,
                },
            };
        }
    }
    return Value.err("Field does not exist", "The field does not exist", null);
}

pub fn castToEnum(self: Self, value: Value) !SymbolTable.SymbolValue {
    const value_as_tag = try rt.castToSymbolValue(self.allocator, value, self.tagged.value);
    for (self.fields) |*f| {
        if (f.value.equal(value_as_tag)) {
            return SymbolTable.SymbolValue{ .enum_instance = Instance{ .field = f, .type_uuid = self.uuid, .global_uuid = self.global_uuid } };
        }
    }
    return error.InvalidCast;
}
