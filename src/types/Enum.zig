const std = @import("std");
const SymbolTable = @import("../SymbolTable.zig");
const TypeVal = @import("../Token.zig").TypeValue;
const Type = @import("../Type.zig");

const rt = @import("../runtime.zig");
const ty = @import("../types.zig");

const Value = rt.Value;
const Result = rt.Result;

var global_uuid: u64 = 0; // the next uuid to be used

pub const Field = struct {
    name: []const u8,
    value: SymbolTable.SymbolValue, // the actual value
    backing_value: u64, // for quick comparisons
};

// Enum Instance aka doing EnumType.name will return instance with said field
pub const Instance = struct {
    field: Field,
    type_uuid: u64, // not from the same enum
    strict: bool, // type information whether it is strict or not, use in symbol table

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

tagged: Type = Type.init(.i32, null),

pub fn init(allocator: std.mem.Allocator, name: []const u8, fields: []const Field, tagged: Type) Self {
    const uuid = global_uuid;
    global_uuid += 1;
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
        .is_generic = false,
    };
}
