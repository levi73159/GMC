const std = @import("std");
const SymbolTable = @import("SymbolTable.zig");
const rt = @import("runtime.zig");

const functions = @import("builtin_tools/functions.zig");

pub fn initRootSymbols(allocator: std.mem.Allocator) !SymbolTable {
    var symbols = SymbolTable.init(allocator);

    const typeinfo = @typeInfo(functions);
    inline for (typeinfo.@"struct".decls) |decl| {
        // check if public and if so add it to the symbol table
        try symbols.add(decl.name, .{
            .is_const = true,
            .value = .{
                .func = .{ .bultin = .{
                    .name = decl.name,
                    .func = @field(functions, decl.name),
                } },
            },
        });
    }

    return symbols;
}
