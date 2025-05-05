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
    if (base.static) return none();

    const stdout = std.io.getStdOut().writer();

    for (args, 0..) |arg, i| {
        stdout.print("{}", .{arg}) catch return errHeap(base.allocator, "IO Error", "Failed to write to stdout", .{});
        if (i != args.len - 1) {
            stdout.print(" ", .{}) catch return errHeap(base.allocator, "IO Error", "Failed to write to stdout", .{});
        }
    }
    return none();
}

pub fn println(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args, base);
    if (base.static) return none();

    const stdout = std.io.getStdOut().writer();

    for (args, 0..) |arg, i| {
        stdout.print("{}", .{arg}) catch return errHeap(base.allocator, "IO Error", "Failed to write to stdout", .{});
        if (i != args.len - 1) {
            stdout.print(" ", .{}) catch return errHeap(base.allocator, "IO Error", "Failed to write to stdout", .{});
        }
    }
    stdout.print("\n", .{}) catch return errHeap(base.allocator, "IO Error", "Failed to write to stdout", .{});
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

pub fn exit(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args, base);

    if (args.len == 0) std.process.exit(0);
    if (args.len != 1) return err("exit", "Expected 1 or 0 arguments");

    const code = rt.safeIntCast(u8, args[0]) catch {
        return errHeap(base.allocator, "Invalid Cast", "Can't cast {s} to an exit code (0-255)", .{@tagName(args[0])});
    };

    std.process.exit(code);
}

pub fn len(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args, base);

    if (args.len != 1) return err("len", "Expected 1 argument");

    const arg = args[0];
    if (arg == .string) {
        return val(.{ .integer = arg.string.value.len });
    }

    return err("len", "Expected string");
}

pub fn input(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args, base);
    if (base.static) return val(.{ .string = rt.String.init(base.allocator, "", false) catch unreachable }); // not a static function

    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    const prompt: []const u8 = if (args.len == 0) "" else args[0].string.value;
    stdout.print("{s} ", .{prompt}) catch return err("IO Error", "Failed to write to stdout");

    const in = stdin.readUntilDelimiterOrEofAlloc(base.allocator, '\n', 1024) catch |e| return errHeap(base.allocator, "IO Error", "Failed to read from stdin: {s}", .{@errorName(e)});
    if (in) |str| {
        return val(.{ .string = rt.String.init(base.allocator, str, true) catch unreachable });
    } else {
        return val(.{ .string = rt.String.init(base.allocator, "", false) catch unreachable });
    }

    return none();
}
