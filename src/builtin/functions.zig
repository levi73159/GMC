const std = @import("std");
const Interpreter = @import("../Interpreter.zig");
const SymbolTable = @import("../SymbolTable.zig");
const tools = @import("tools.zig");

const rt = @import("../runtime.zig");
const ty = @import("../types.zig");

const end = tools.end;
const errHeap = tools.errHeap;
const err = tools.err;
const none = tools.none;
const val = tools.val;

pub usingnamespace @import("casts.zig");

pub fn print(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args);
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
    defer end(args);
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
    defer end(args);

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
    return rt.Result.errHeap(base.allocator, name.value, raw_msg, null);
}

pub fn exit(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args);

    if (args.len == 0) std.process.exit(0);
    if (args.len != 1) return err("exit", "Expected 1 or 0 arguments");

    const code = rt.safeIntCast(u8, args[0]) catch {
        return errHeap(base.allocator, "Invalid Cast", "Can't cast {s} to an exit code (0-255)", .{@tagName(args[0])});
    };

    std.process.exit(code);
}

pub fn len(args: []const rt.Value, _: Interpreter) rt.Result {
    defer end(args);

    if (args.len != 1) return err("len", "Expected 1 argument");

    const arg = args[0].depointerizeToValue();
    const result = switch (arg) {
        .string => |s| val(.{ .integer = s.value.len }),
        .list => |l| blk: {
            break :blk val(.{ .integer = l.items.len });
        },
        else => err("len", "Expected a sizeable type"),
    };

    return result;
}

pub fn input(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args);
    if (base.static) return val(.{ .string = ty.String.init(base.allocator, "", false) catch unreachable }); // not a static function

    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    const prompt: []const u8 = if (args.len == 0) "" else args[0].string.value;
    stdout.print("{s}", .{prompt}) catch return err("IO Error", "Failed to write to stdout");

    const in = stdin.readUntilDelimiterOrEofAlloc(base.allocator, '\n', 1024) catch |e| return errHeap(base.allocator, "IO Error", "Failed to read from stdin: {s}", .{@errorName(e)});
    if (in) |str| {
        return val(.{ .string = ty.String.init(base.allocator, str, true) catch unreachable });
    } else {
        return val(.{ .string = ty.String.init(base.allocator, "", false) catch unreachable });
    }

    return none();
}

pub fn eval(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args);

    const Lexer = @import("../Lexer.zig");
    const Parser = @import("../Parser.zig");

    if (args.len != 1) return err("eval", "Expected 1 argument");
    if (args[0] != .string) return err("eval", "Expected string");

    var arena = std.heap.ArenaAllocator.init(base.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const expr = args[0].string;
    var lexer = Lexer.init(expr.value);
    const tokens = lexer.makeTokens(allocator) catch |e| {
        return errHeap(base.allocator, "Lexer Error", "Failed to evalulate: {s} due to {s}", .{ expr.value, @errorName(e) });
    };

    var parser = Parser.init(tokens, allocator, base.allocator);
    const nodes = parser.parse() catch |e| {
        return errHeap(base.allocator, "Parser Error", "Failed to evalulate: {s} due to {s}", .{ expr.value, @errorName(e) });
    };

    var symbols = SymbolTable.init(base.allocator);
    symbols.parent = base.symbols;
    defer symbols.deinit();

    const interpreter = base.newScope(&symbols, base.allocator);
    const result = interpreter.evalResult(nodes);

    if (result == .signal) return errHeap(base.allocator, "Eval Error", "Failed to evalulate: {s} due to \"Returned a Signal\"", .{expr.value});
    const value = result.value;
    if (value == .runtime_error) return errHeap(base.allocator, "Eval Error", "Eval returned an error: {s}", .{value.runtime_error.msg});
    return result;
}
