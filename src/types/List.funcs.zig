const std = @import("std");
const tools = @import("../builtin/tools.zig");
const Interpreter = @import("../Interpreter.zig");

const rt = @import("../runtime.zig");

const end = tools.end;
const errHeap = tools.errHeap;
const err = tools.err;
const none = tools.none;
const val = tools.val;

pub fn append(args: []const rt.Value, _: Interpreter) rt.Result {
    defer end(args);

    if (args.len != 2) return err("append", "Expected 2 arguments");

    const list = args[0].list;
    const item = args[1];

    list.append(item) catch |e| switch (e) {
        error.OutOfMemory => @panic("out of memory"),
        error.ImmutableList => return err("Immutable List", "Can't append to an immutable list"),
        error.InvalidCast => return err("Invalid Type", "Can't append value to list because of invalid type"),
        error.NotGeneric => return err("Not Generic", "The type is not generic"),
        error.ExpectedGeneric => return err("Expected Generic", "The type is needs a generic type"),
        else => return err("Unknown Error", "PANIC"),
    };

    return none();
}

pub fn clear(args: []const rt.Value, _: Interpreter) rt.Result {
    defer end(args);

    if (args.len != 1) return err("clear", "Expected 1 argument");

    const list = args[0].list;
    list.clear();
    return none();
}

pub fn pop(args: []const rt.Value, _: Interpreter) rt.Result {
    defer end(args);

    if (args.len != 1) return err("pop", "Expected 1 argument");

    const list = args[0].list;

    const item = list.pop() catch |e| switch (e) {
        error.ImmutableList => return err("Immutable List", "Can't pop from an immutable list"),
    };
    if (item == null) return err("Empty List", "Can't pop from an empty list");

    return val(rt.castToValueNoRef(item.?.value));
}
