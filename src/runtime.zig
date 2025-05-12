const std = @import("std");

const Pos = @import("DebugPos.zig");
const tree = @import("tree.zig");
const SymbolTable = @import("SymbolTable.zig");
const TypeVal = @import("Token.zig").TypeValue;
const Node = tree.Node;
const Intrepreter = @import("Interpreter.zig");
const Params = tree.FuncParam;
const Type = @import("Type.zig");

const types = @import("types.zig");
const Error = types.Error;

const rtvalues = @import("rtvalues.zig");
pub const Value = rtvalues.Value;
pub const Result = rtvalues.Result;
pub const SymbolPtr = rtvalues.SymbolPtr;
pub const Signal = rtvalues.Signal;

pub fn safeIntCast(comptime TO: type, v_n: Value) !TO {
    const v = v_n.depointerizeToValue();

    // check if value is to big to fit in the type
    const max = std.math.maxInt(TO);
    const min = std.math.minInt(TO);

    switch (v) {
        .integer => |i| {
            if (i > max) return error.InvalidCast;
            if (i < min) return error.InvalidCast;
            return @intCast(i);
        },
        .float => |f| {
            if (f > max) return error.InvalidCast;
            if (f < min) return error.InvalidCast;
            const int: i64 = @intFromFloat(f);
            return @intCast(int);
        },
        .char => |c| {
            if (c > max) return error.InvalidCast;
            if (c < min) return error.InvalidCast;
            return @intCast(c);
        },
        .none => return 0, // cast none types to 0
        else => return error.InvalidCast,
    }
}

pub fn safeFloatCast(comptime TO: type, v_n: Value) !TO {
    const v = v_n.depointerizeToValue();

    const max = std.math.floatMax(TO);
    const min = -std.math.floatMax(TO);

    switch (v) {
        .integer => |i| {
            const float: f64 = @floatFromInt(i);
            if (float > max) return error.InvalidCast;
            if (float < min) return error.InvalidCast;
            return @floatCast(float);
        },
        .float => |f| {
            if (f > max) return error.InvalidCast;
            if (f < min) return error.InvalidCast;
            return @floatCast(f);
        },
        .char => |c| {
            const float: f64 = @floatFromInt(c);
            if (float > max) return error.InvalidCast;
            if (float < min) return error.InvalidCast;
            return @floatCast(float);
        },
        .none => return 0.0, // cast none types to 0
        else => return error.InvalidCast,
    }
}

pub fn safeBoolCast(v: Value) !bool {
    const new = v.convertToBool(); // will handle pointer
    if (new == .runtime_error) return error.InvalidCast;
    return new.boolean;
}

pub fn safeStrCast(allocator: std.mem.Allocator, v_n: Value) !types.String {
    const v = v_n.depointerizeToValue();

    return switch (v) {
        .string => |s| s,
        .char => |c| types.String{ .allocator = allocator, .value = &[_]u8{c}, .mem_type = .stack },
        .none => types.String{ .allocator = allocator, .value = "", .mem_type = .stack },
        .integer => blk: {
            const char = try safeIntCast(u8, v);
            break :blk types.String{ .allocator = allocator, .value = &[_]u8{char}, .mem_type = .stack };
        },
        .func => |f| types.String{ .allocator = allocator, .value = f.getName(), .mem_type = .stack }, // cast function to string == func name????
        else => return error.InvalidCast,
    };
}

pub fn safeListCast(allocator: std.mem.Allocator, v_n: Value, immutable: bool) anyerror!*types.List {
    const v = v_n.depointerizeToValue();
    return switch (v) {
        .string => |s| blk: {
            const new = types.List.init(allocator, immutable);
            new.resize(s.value.len) catch unreachable;
            for (s.value) |char| {
                try new.append(Value{ .char = char });
            }
            break :blk new;
        },
        .list => |l| {
            if (l.immutable == immutable) return l;
            return l.recursiveMutablity(immutable);
        },
        .none => types.List.init(allocator, immutable),
        else => return error.InvalidCast,
    };
}

fn typeInfer(v: Value) !SymbolTable.SymbolValue {
    return switch (v) {
        .integer => SymbolTable.SymbolValue{ .i64 = try safeIntCast(i64, v) },
        .float => |f| SymbolTable.SymbolValue{ .float = f },
        .char => |c| SymbolTable.SymbolValue{ .char = c },
        .string => |s| SymbolTable.SymbolValue{ .string = s },
        .boolean => |b| SymbolTable.SymbolValue{ .bool = b },
        .func => |f| SymbolTable.SymbolValue{ .func = f },
        .list => |l| SymbolTable.SymbolValue{ .list = l },
        .none => SymbolTable.SymbolValue{ .void = {} },
        else => return error.InvalidCast,
    };
}

pub fn castToSymbolValue(allocator: std.mem.Allocator, v_n: Value, ty: TypeVal) !SymbolTable.SymbolValue {
    const v = v_n.depointerizeToValue();

    const SymVal = SymbolTable.SymbolValue;
    return switch (ty) {
        .i8 => SymVal{ .i8 = try safeIntCast(i8, v) },
        .i16 => SymVal{ .i16 = try safeIntCast(i16, v) },
        .i32 => SymVal{ .i32 = try safeIntCast(i32, v) },
        .i64 => SymVal{ .i64 = try safeIntCast(i64, v) },
        .u8 => SymVal{ .u8 = try safeIntCast(u8, v) },
        .u16 => SymVal{ .u16 = try safeIntCast(u16, v) },
        .u32 => SymVal{ .u32 = try safeIntCast(u32, v) },
        .u64 => SymVal{ .u64 = try safeIntCast(u64, v) },

        .int => SymVal{ .i32 = try safeIntCast(i32, v) },
        .float => SymVal{ .float = try safeFloatCast(f64, v) },
        .bool => SymVal{ .bool = try safeBoolCast(v) },
        .str => SymVal{ .string = try safeStrCast(allocator, v) },
        .char => SymVal{ .char = try safeIntCast(u8, v) },
        .void => SymVal{ .void = {} },
        .list => SymVal{ .list = try safeListCast(allocator, v, false) },
        .imlist => SymVal{ .list = try safeListCast(allocator, v, true) }, // immutable list
        .any => typeInfer(v),
    };
}

// values are temporary and are not stored therefore we wanna clone them if needed
pub fn castToValue(v: SymbolTable.SymbolValue) Value {
    return castToValueNoRef(v).ref();
}

pub fn castToValueNoRef(v: SymbolTable.SymbolValue) Value {
    return switch (v) {
        .i8 => |i| Value{ .integer = i },
        .i16 => |i| Value{ .integer = i },
        .i32 => |i| Value{ .integer = i },
        .i64 => |i| Value{ .integer = i },
        .u8 => |i| Value{ .integer = i },
        .u16 => |i| Value{ .integer = i },
        .u32 => |i| Value{ .integer = i },
        .u64 => |i| Value{ .integer = i },
        .float => |f| Value{ .float = f },

        .bool => |b| Value{ .boolean = b },
        .string => |s| Value{ .string = s },
        .char => |c| Value{ .char = c },
        .func => |f| Value{ .func = f },
        .void => Value{ .none = {} },
        .list => |l| Value{ .list = l },
    };
}

pub fn getTypeValFromSymbolValue(v: SymbolTable.SymbolValue) !TypeVal {
    return switch (v) {
        .i8 => TypeVal.i8,
        .i16 => TypeVal.i16,
        .i32 => TypeVal.i32,
        .i64 => TypeVal.i64,
        .u8 => TypeVal.u8,
        .u16 => TypeVal.u16,
        .u32 => TypeVal.u32,
        .u64 => TypeVal.u64,
        .float => TypeVal.float,

        .bool => TypeVal.bool,
        .string => TypeVal.str,
        .char => TypeVal.char,
        .void => TypeVal.void,
        .list => TypeVal.list,
        .func => return error.InvalidType, // func does not havea typeval
    };
}

pub fn checkRuntimeError(value_n: Value, orgin: ?*const Node) ?Result {
    const value = value_n.depointerizeToValue();
    if (value == .runtime_error) {
        var err = value.runtime_error;
        if (err.pos == null) err.pos = if (orgin) |o| o.getPos() else null;
        return Result{ .value = .{ .runtime_error = err } };
    }
    return null;
}

// returns a runtime error if there is one
pub fn checkRuntimeErrorOrSignal(result: Result, orgin: *const Node) ?Result {
    switch (result) {
        .value => |v| return checkRuntimeError(v, orgin),
        .signal => |s| return Result{ .signal = s },
    }
    return null;
}

pub fn castToIndex(v_n: Value, len: usize) !usize {
    const v = v_n.depointerizeToValue();

    return switch (v) {
        .integer => |int| blk: {
            if (int < 0) break :blk len - @as(usize, @intCast(int * -1)); //
            break :blk @intCast(int);
        },
        .float => blk: {
            const int = try safeIntCast(i65, v);
            if (int < 0) break :blk len + @as(usize, @intCast(int * -1));
            break :blk @intCast(int);
        },
        else => error.InvalidCast,
    };
}

fn setGeneric(gtype: Type, symval: SymbolTable.SymbolValue) !void {
    if (gtype.getGenericInfo()) |info| { // it is generic
        const generic_type: Type =
            if (gtype.generic_type) |gen|
                gen.*
            else if (info.default) |default|
                Type.init(default, null)
            else
                return error.ExpectedGeneric;

        try symval.setGenericType(generic_type);
    } else {
        if (gtype.generic_type) |_| return error.NotGeneric;
    }
}

pub fn castToType(allocator: std.mem.Allocator, v_n: Value, ty: Type) !SymbolTable.SymbolValue {
    const v = v_n.depointerizeToValue();

    const symval = try castToSymbolValue(allocator, v.clone(), ty.value);
    errdefer symval.deinit();

    try setGeneric(ty, symval);
    return symval;
}

pub fn castToTypeWithErrorMessage(allocator: std.mem.Allocator, v_n: Value, ty: Type, symbol_value: *SymbolTable.SymbolValue) Result {
    var returning_error: bool = false;
    const v = v_n.depointerizeToValue();

    symbol_value.* = castToSymbolValue(allocator, v, ty.value) catch {
        returning_error = true;
        return Result.err("Invalid Cast", "Can't cast value to type (could be due to because the value is too big or small)", null);
    };
    defer if (returning_error) symbol_value.deinit();

    setGeneric(ty, symbol_value.*) catch |err| switch (err) {
        error.OutOfMemory => std.debug.panic("OUT OF MEMORY!!!", .{}),
        error.InvalidCast => return Result.errPrint(
            allocator,
            "Invalid Cast",
            "Can't convert {0s} to a {0s}<{1s}>",
            .{ @tagName(ty.value), ty },
            null,
        ),
        error.NotGeneric => return Result.err("Not Generic", "The type is not generic", null),
        error.ExpectedGeneric => return Result.err("Expected Generic", "The type is needs a generic type", null),
        else => return Result.err("Unknown Error", "PANIC", null),
    };

    return Result.none();
}
