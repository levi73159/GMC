const std = @import("std");
const rt = @import("../runtime.zig");
const Interpreter = @import("../Interpreter.zig");

pub fn val(value: rt.Value) rt.Result {
    return rt.Result.val(value);
}

pub fn none() rt.Result {
    return rt.Result.none();
}

pub fn err(name: []const u8, msg: []const u8) rt.Result {
    return rt.Result.err(name, msg, null);
}

pub fn errHeap(allocator: std.mem.Allocator, name: []const u8, comptime fmt: []const u8, args: anytype) rt.Result {
    const msg = std.fmt.allocPrint(allocator, fmt, args) catch unreachable;
    return rt.Result.errHeap(name, msg, null);
}

pub fn end(args: []const rt.Value, base: Interpreter) void {
    for (args) |arg| arg.deinit(base.allocator);
}
