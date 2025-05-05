const std = @import("std");
const Interpreter = @import("../Interpreter.zig");
const tools = @import("tools.zig");

const rt = @import("../runtime.zig");
const ty = @import("../types.zig");

const end = tools.end;
const errHeap = tools.errHeap;
const err = tools.err;
const none = tools.none;
const val = tools.val;

pub fn asInt(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args, base);

    if (args.len != 1) return err("asInt", "Expected 1 argument");

    const arg = args[0];
    if (arg == .integer) {
        return val(arg);
    }

    const integer = rt.safeIntCast(i65, arg) catch {
        return errHeap(base.allocator, "Invalid Cast", "Can't cast {s} to int", .{@tagName(arg)});
    };

    return val(.{ .integer = integer });
}

pub fn asFloat(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args, base);

    if (args.len != 1) return err("asFloat", "Expected 1 argument");

    const arg = args[0];
    if (arg == .float) {
        return val(arg);
    }

    const float = rt.safeFloatCast(f64, arg) catch {
        return errHeap(base.allocator, "Invalid Cast", "Can't cast {s} to float", .{@tagName(arg)});
    };

    return val(.{ .float = float });
}

pub fn asChar(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args, base);

    if (args.len != 1) return err("asChar", "Expected 1 argument");

    const arg = args[0];
    if (arg == .char) {
        return val(arg);
    }

    const char = rt.safeIntCast(u8, arg) catch {
        return errHeap(base.allocator, "Invalid Cast", "Can't cast {s} to char", .{@tagName(arg)});
    };

    return val(.{ .char = char });
}

pub fn asString(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args, base);

    if (args.len != 1) return err("asString", "Expected 1 argument");

    const arg = args[0];
    if (arg == .string) {
        return val(arg);
    }

    const string = rt.safeStrCast(base.allocator, arg) catch {
        return errHeap(base.allocator, "Invalid Cast", "Can't cast {s} to string", .{@tagName(arg)});
    };

    return val(.{ .string = string });
}

pub fn asBool(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args, base);

    if (args.len != 1) return err("asBool", "Expected 1 argument");

    const arg = args[0];
    if (arg == .boolean) {
        return val(arg);
    }

    const b = rt.safeBoolCast(arg) catch {
        return errHeap(base.allocator, "Invalid Cast", "Can't cast {s} to bool", .{@tagName(arg)});
    };

    return val(.{ .boolean = b });
}

pub fn toString(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args, base);

    if (args.len == 0) {
        return val(.{ .string = ty.String.init(base.allocator, "", false) catch unreachable });
    }
    if (args.len != 1) return err("toString", "Expected 1 argument");

    const arg = args[0];
    if (arg == .string) {
        return val(arg);
    }

    const base_string = std.fmt.allocPrint(base.allocator, "{}", .{arg}) catch unreachable;
    const string = ty.String.init(base.allocator, base_string, true) catch unreachable;
    return val(.{ .string = string });
}

pub fn toInt(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args, base);

    if (args.len == 0) return val(.{ .integer = 0 });
    if (args.len != 1) return err("toInt", "Expected 1 argument");

    const arg = args[0];
    if (arg != .string) return err("toInt", "Expected string. Please use asInt to cast a value to int");

    const str = arg.string;
    const int = std.fmt.parseInt(i65, str.value, 10) catch {
        return errHeap(base.allocator, "Invalid Cast", "Can't cast \"{s}\" to int", .{str.value});
    };
    return val(.{ .integer = int });
}

pub fn toFloat(args: []const rt.Value, base: Interpreter) rt.Result {
    defer end(args, base);

    if (args.len == 0) return val(.{ .float = 0 });
    if (args.len != 1) return err("toFloat", "Expected 1 argument");

    const arg = args[0];
    if (arg != .string) return err("toFloat", "Expected string. Please use asFloat to cast a value to float");

    const str = arg.string;
    const float = std.fmt.parseFloat(f64, str.value) catch {
        return errHeap(base.allocator, "Invalid Cast", "Can't cast \"{s}\" to float", .{str.value});
    };
    return val(.{ .float = float });
}
