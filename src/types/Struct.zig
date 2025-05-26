const std = @import("std");
const SymbolTable = @import("../SymbolTable.zig");
const TypeVal = @import("../Token.zig").TypeValue;
const Type = @import("../Type.zig");

const rt = @import("../runtime.zig");
const ty = @import("../types.zig");

const Value = rt.Value;

const Self = @This();

var struct_uuid: u64 = 0;

name: []const u8,
inner: *SymbolTable, // no need to make the entire struct on the heap, so better if we make this a pointer instead of the struct

type_uuid: u64 = 0, // for checking if a struct instance is based on this struct blueprint?
global_uuid: u64, // type uuid

pub fn init(name: []const u8, inner: *SymbolTable) Self {
    const uuid = struct_uuid;
    struct_uuid += 1;
    return Self{
        .name = name,
        .inner = inner,
        .global_uuid = 0,
        .type_uuid = uuid,
    };
}

pub fn deinit(self: Self) void {
    self.inner.deinit();
    self.inner.allocator.destroy(self.inner);
}

pub fn asTypeInfo(self: Self) ty.TypeInfo {
    return ty.TypeInfo{
        .define = .{
            .type_name = self.name,
            .base_name = "Struct",
            .size = 0,
            .is_builtin = false,
        },
        .type_uuid = self.type_uuid,
        .global_uuid = self.global_uuid,
        .is_generic = false,
    };
}

pub fn field(self: Self, name: []const u8) Value {
    const sym = self.inner.getPtr(name) orelse return Value.err("Field does not exist", "The field does not exist", null);
    return Value{
        .symbol = .{
            .type = Type.any(), // TODO: change this to the type of the field
            .ptr = sym,
        },
    };
}
