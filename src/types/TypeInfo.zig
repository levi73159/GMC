const std = @import("std");

const TypeDefineData = struct {
    type_name: []const u8, // the actual name, aka if we define an enum type_name will be the enum name
    base_name: []const u8, // the base name of the type if we define an enum, Enum will be the base name
    alias: ?[]const u8 = null,
    size: u32, // in bytes
    is_builtin: bool, // if the tpye is a builtin type
};

define: TypeDefineData, // the type definition data, aka the type name and size
is_generic: bool,
type_uuid: u64, // builtin types have a uuid of zero (specific to the type)
global_uuid: u64, // every type has a global uuid for every time
