const std = @import("std");
const rt = @import("../runtime.zig");
const Interpreter = @import("../Interpreter.zig");

inline fn val(value: rt.Value) rt.Result {
    return rt.Result.val(value);
}

inline fn none() rt.Result {
    return rt.Result.none();
}

fn end(args: []const rt.Value, base: Interpreter) void {
    for (args) |arg| arg.deinit(base.allocator);
}

pub fn print(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args, base);

    for (args, 0..) |arg, i| {
        std.debug.print("{}", .{arg});
        if (i != args.len - 1) {
            std.debug.print(" ", .{});
        }
    }
    return none();
}

pub fn println(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args, base);

    for (args, 0..) |arg, i| {
        std.debug.print("{}", .{arg});
        if (i != args.len - 1) {
            std.debug.print(" ", .{});
        }
    }
    std.debug.print("\n", .{});
    return none();
}
