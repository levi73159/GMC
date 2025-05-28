const std = @import("std");
const SymbolTable = @import("../SymbolTable.zig");
const TypeVal = @import("../Token.zig").TypeValue;
const Type = @import("../Type.zig");

const rt = @import("../runtime.zig");
const ty = @import("../types.zig");

const Value = rt.Value;

const Self = @This();
pub const Field = struct {
    type: Type,
    name: []const u8,
    default: ?Value,
};

pub const Instance = struct {
    symbols: SymbolTable,
    refs: u32 = 1,
    allocator: std.mem.Allocator,

    type_uuid: u64 = 0,
    global_uuid: u64 = 0,
    strict: bool = true,

    pub fn deinit(self: *Instance) void {
        self.refs -|= 1;
        if (self.refs == 0) {
            self.symbols.deinit();
            self.allocator.destroy(self);
        }
    }

    pub fn clone(self: *Instance) *Instance {
        const ptr = self.allocator.create(Instance) catch unreachable;
        ptr.* = Instance{
            .symbols = self.symbols.clone() catch unreachable,
            .refs = self.refs,
            .allocator = self.allocator,
            .type_uuid = self.type_uuid,
            .global_uuid = self.global_uuid,
            .strict = self.strict,
        };
        return ptr;
    }

    pub fn ref(self: *Instance) *Instance {
        self.refs += 1;
        return self;
    }

    pub fn field(self: *Instance, name: []const u8) Value {
        const ptr = self.symbols.getPtr(name) orelse return Value.err("Field does not exist", "The field does not exist", null);
        return Value{
            .symbol = ptr,
        };
    }
};

var struct_uuid: u64 = 0;

name: []const u8,
inner: *SymbolTable, // no need to make the entire struct on the heap, so better if we make this a pointer instead of the struct
fields: []const Field,

type_uuid: u64 = 0, // for checking if a struct instance is based on this struct blueprint?
global_uuid: u64, // type uuid

pub fn init(name: []const u8, inner: *SymbolTable, fields: []const Field) Self {
    const uuid = struct_uuid;
    struct_uuid += 1;
    return Self{
        .name = name,
        .inner = inner,
        .fields = fields,
        .global_uuid = 0,
        .type_uuid = uuid,
    };
}

pub fn deinit(self: Self) void {
    self.inner.allocator.free(self.fields); // free fields
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
        .symbol = sym,
    };
}

pub fn makeNone(self: Self, allocator: std.mem.Allocator) !*Instance {
    const ptr = allocator.create(Instance) catch unreachable;
    var symbols = SymbolTable.init(allocator);
    symbols.parent = self.inner;

    for (self.fields) |f| {
        try symbols.add(f.name, SymbolTable.Symbol{
            .value = try rt.castToType(allocator, f.default orelse .none, f.type),
            .type = f.type,
            .is_const = false, // fields are never const
        });
    }

    ptr.* = Instance{
        .symbols = symbols, // no need to clone
        .allocator = allocator,
        .global_uuid = self.global_uuid,
        .type_uuid = self.type_uuid,
    };
    return ptr;
}
