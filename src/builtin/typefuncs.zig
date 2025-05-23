const std = @import("std");
const Interpreter = @import("../Interpreter.zig");
const SymbolTable = @import("../SymbolTable.zig");
const tools = @import("tools.zig");

const rt = @import("../runtime.zig");
const ty = @import("../types.zig");
const Type = @import("../Type.zig");

const end = tools.end;
const errHeap = tools.errHeap;
const err = tools.err;
const none = tools.none;
const val = tools.val;

pub fn getName(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args);
    if (args.len != 1) return err("getTagName", "Expected 1 argument");
    const arg = args[0];

    return switch (arg) {
        .enum_instance => |ei| return val(.{ .string = ty.String.init(base.allocator, ei.field.name, false) catch unreachable }),
        .@"enum" => |e| val(.{ .string = ty.String.init(base.allocator, e.enum_name, false) catch unreachable }),
        else => errHeap(base.allocator, "getTagName", "Can't get name of {s}", .{@tagName(arg)}),
    };
}
