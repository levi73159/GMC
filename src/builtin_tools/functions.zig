const std = @import("std");
const rt = @import("../runtime.zig");
const Interpreter = @import("../Interpreter.zig");
const tools = @import("tools.zig");

const end = tools.end;
const errHeap = tools.errHeap;
const err = tools.err;
const none = tools.none;
const val = tools.val;

pub usingnamespace @import("casts.zig");

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

pub fn throw(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args, base);

    if (args.len != 2) return err("throw", "Expected 2 arguments");
    const err_name = args[0];
    const err_msg = args[1];

    const name = rt.safeStrCast(base.allocator, err_name) catch {
        return errHeap(base.allocator, "Invalid Cast", "Can't cast {s} to string", .{@tagName(err_name)});
    };
    const msg = rt.safeStrCast(base.allocator, err_msg) catch {
        return errHeap(base.allocator, "Invalid Cast", "Can't cast {s} to string", .{@tagName(err_msg)});
    };

    const raw_msg = base.allocator.dupe(u8, msg.value) catch unreachable;
    return rt.Result.errHeap(name.value, raw_msg, null);
}
