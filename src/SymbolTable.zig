const std = @import("std");
const rt = @import("runtime.zig");
const types = @import("types.zig");
const Type = @import("Type.zig");
const TypeInfo = types.TypeInfo;

const Self = @This();

pub const SymbolValue = union(enum) {
    // structure types (aka struct, class, enum)
    @"enum": types.Enum,
    enum_instance: types.Enum.Instance,

    @"struct": types.Struct,
    struct_instance: *types.Struct.Instance,

    // value types
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
    type: TypeInfo,

    pub fn deinit(self: SymbolValue) void {
        switch (self) {
            .string => |s| s.deinit(),
            .list => |l| l.deinit(),
            .@"enum" => |e| e.deinit(),
            .@"struct" => |s| s.deinit(),
            .struct_instance => |s| s.deinit(),
            else => {},
        }
    }

    pub fn clone(self: SymbolValue) SymbolValue {
        return switch (self) {
            .string => |s| SymbolValue{ .string = s.clone() },
            .list => |l| SymbolValue{ .list = l.clone() },
            .struct_instance => |s| SymbolValue{ .struct_instance = s.clone() },
            else => self,
        };
    }

    pub fn ref(self: SymbolValue) SymbolValue {
        return switch (self) {
            .string => |s| SymbolValue{ .string = s.ref() },
            .list => |l| SymbolValue{ .list = l.ref() },
            .struct_instance => |s| SymbolValue{ .struct_instance = s.ref() },
            else => self,
        };
    }

    pub fn setGenericType(self: SymbolValue, ty: Type) !void {
        switch (self) {
            .list => |l| try l.setGenericType(ty),
            else => return error.NotGeneric,
        }
    }

    // if you want a better equal with errors, cast to a RTValue
    pub fn equal(self: SymbolValue, other: SymbolValue) bool {
        const fields = @typeInfo(@TypeOf(self)).@"union".fields;
        switch (self) {
            .u8, .u16, .u32, .u64, .i8, .i16, .i32, .i64, .char => {
                const lhs: i65 = inline for (fields) |field| {
                    if (@typeInfo(field.type) == .int and std.mem.eql(u8, field.name, @tagName(self))) {
                        break @intCast(@field(self, field.name));
                    }
                } else unreachable;
                switch (other) {
                    .u8, .u16, .u32, .u64, .i8, .i16, .i32, .i64, .char => {
                        const rhs: i65 = inline for (fields) |field| {
                            if (@typeInfo(field.type) == .int and std.mem.eql(u8, field.name, @tagName(other))) {
                                break @intCast(@field(other, field.name));
                            }
                        } else unreachable;
                        return lhs == rhs;
                    },
                    .float => |rhs| return @as(f64, @floatFromInt(lhs)) == rhs,
                    .void => return lhs == 0,
                    else => return false,
                }
            },
            .float => |lhs| {
                switch (other) {
                    .u8, .u16, .u32, .u64, .i8, .i16, .i32, .i64, .char => {
                        inline for (fields) |field| {
                            if (@typeInfo(field.type) == .int and std.mem.eql(u8, field.name, @tagName(other))) {
                                const rhs: f64 = @floatFromInt(@field(other, field.name));
                                return lhs == rhs;
                            }
                        } else unreachable;
                    },
                    .float => |rhs| return lhs == rhs,
                    .void => return lhs == 0.0,
                    else => return false,
                }
            },
            .bool => |lhs| {
                switch (other) {
                    .bool => |rhs| {
                        return lhs == rhs;
                    },
                    .void => return lhs == false,
                    .u8, .u16, .u32, .u64, .i8, .i16, .i32, .i64, .float, .char => {
                        const rhs: bool = inline for (fields) |field| {
                            if (@typeInfo(field.type) == .int and std.mem.eql(u8, field.name, @tagName(other))) {
                                break @field(other, field.name) != 0;
                            }
                        } else unreachable;
                        return lhs == rhs;
                    },
                    else => return false,
                }
            },
            .void => return false,
            .string => |lhs| {
                switch (other) {
                    .string => |rhs| {
                        return lhs.equal(rhs);
                    },
                    else => return false,
                }
            },
            .list => |lhs| {
                switch (other) {
                    .list => |rhs| {
                        const result = lhs.equal(rhs);
                        if (result != .boolean) return false;
                        return result.boolean;
                    },
                    else => return false,
                }
            },
            .@"enum" => |lhs| {
                switch (other) {
                    .@"enum" => |rhs| {
                        return lhs.uuid == rhs.uuid;
                    },
                    else => return false,
                }
            },
            .enum_instance => |lhs| {
                switch (other) {
                    .enum_instance => |rhs| {
                        return lhs.equal(rhs);
                    },
                    else => return false,
                }
            },
            else => return false, // TODO: structs
        }
    }

    pub fn size(self: SymbolValue) u32 {
        return switch (self) {
            .u8, .i8, .char, .bool => 1,
            .u16, .i16 => 2,
            .u32, .i32 => 4,
            .u64, .i64 => 8,
            .float => 8,
            .string => |s| @truncate(s.value.len),
            .list => |l| l.size(),
            .func => 8, // act as pointer
            .void, .type => 0, // comptime or none size
            .@"enum" => |e| e.tagged.size() orelse 8,
            .enum_instance => |e| e.field.value.size(),
            .@"struct" => 0,
            .struct_instance => 0,
        };
    }

    pub fn format(
        self: SymbolValue,
        comptime fmt: []const u8,
        opts: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        return rt.castToValueNoRef(self).format(fmt, opts, writer);
    }
};
pub const SymbolType = std.meta.Tag(SymbolValue);

pub const Symbol = struct {
    is_const: bool,
    value: SymbolValue,
    type: ?Type = null,

    pub fn deinit(self: Symbol) void {
        self.value.deinit();
    }

    pub fn clone(self: Symbol) Symbol {
        return Symbol{ .is_const = self.is_const, .value = self.value.clone(), .type = self.type };
    }

    pub fn ref(self: Symbol) Symbol {
        return Symbol{ .is_const = self.is_const, .value = self.value.ref(), .type = self.type };
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

pub fn addGetPtr(self: *Self, name: []const u8, symbol: Symbol) !*Symbol {
    const result = try self.table.getOrPut(try self.allocator.dupe(u8, name));
    if (result.found_existing) return error.SymbolAlreadyExists;

    result.value_ptr.* = symbol;
    return result.value_ptr;
}

pub fn set(self: *Self, name: []const u8, value: SymbolValue) !void {
    const symbol = self.getPtr(name) orelse return error.SymbolDoesNotExist;
    if (symbol.is_const) return error.SymbolIsImmutable;
    // check if the type is the same as value
    const ty = std.meta.activeTag(symbol.value);
    const val = std.meta.activeTag(value);

    // type mismatch check
    if (ty != val) return error.InvalidTypes;
    switch (symbol.value) {
        .enum_instance => |e| {
            if (e.strict == true and e.type_uuid != value.enum_instance.type_uuid) return error.InvalidTypes;
        },
        .struct_instance => |s| {
            if (s.strict == true and s.type_uuid != value.struct_instance.type_uuid) return error.InvalidTypes;
        },
        else => {},
    }

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

pub fn clone(self: *Self) !Self {
    var self_clone = Self{
        .allocator = self.allocator,
        .parent = self.parent,
        .table = std.StringHashMap(Symbol).init(self.allocator),
    };

    var it = self.table.iterator();
    while (it.next()) |entry| {
        try self_clone.table.put(try self.allocator.dupe(u8, entry.key_ptr.*), entry.value_ptr.clone());
    }
    return self_clone;
}
