const std = @import("std");

const Pos = @import("DebugPos.zig");
const tree = @import("tree.zig");
const SymbolTable = @import("SymbolTable.zig");
const TypeVal = @import("Token.zig").TypeValue;
const Node = tree.Node;
const Intrepreter = @import("Interpreter.zig");
const Params = tree.FuncParam;

const types = @import("types.zig");
const Error = types.Error;

pub const Signal = union(enum) {
    @"return": Value,
    @"break": Value,
    @"continue": void,
};

pub const Value = union(enum) {
    integer: i65,
    float: f64,
    boolean: bool,
    string: types.String,
    char: u8,
    func: types.Function,
    list: *types.List,
    none,
    runtime_error: Error,

    // converts the value to a boolean value
    pub fn convertToBool(self: Value) Value {
        return switch (self) {
            .integer => |i| Value{ .boolean = i != 0 },
            .float => |f| Value{ .boolean = f != 0.0 },
            .string => |s| Value{ .boolean = s.value.len > 0 },
            .char => |c| Value{ .boolean = c != 0 },
            .none => Value{ .boolean = false }, // none == false
            .func => Value{ .boolean = true }, // func == true because it have a value
            .list => |l| Value{ .boolean = l.items.len > 0 },
            .boolean, .runtime_error => self,
        };
    }

    pub fn str(node: tree.String, force_heap: bool, allocator: std.mem.Allocator) !Value {
        if (force_heap) {
            const inner = try allocator.create(types.String.Inner);
            inner.* = types.String.Inner{ .refs = 1 };
            return Value{
                .string = types.String{
                    .value = try allocator.dupe(u8, node.n),
                    .mem_type = .{ .heap = inner },
                    .allocator = allocator,
                },
            };
        }

        if (node.allocated == .stack) {
            return Value{ .string = types.String{
                .value = node.n,
                .mem_type = .stack,
                .allocator = allocator,
            } };
        }

        const inner = try allocator.create(types.String.Inner);
        inner.* = types.String.Inner{ .refs = 1 };

        return Value{ .string = types.String{
            .value = try allocator.dupe(u8, node.n),
            .mem_type = .{ .heap = inner },
            .allocator = allocator,
        } };
    }

    pub fn clone(self: Value) Value {
        return switch (self) {
            .string => |s| Value{ .string = s.clone() },
            .list => |l| Value{ .list = l.clone() },
            else => self,
        };
    }

    pub fn deinit(self: Value, allocator: std.mem.Allocator) void {
        switch (self) {
            .string => |s| s.deinit(),
            .runtime_error => |e| e.deinit(allocator),
            .list => |l| l.deinit(),
            else => {},
        }
    }

    pub fn err(msg: []const u8, extra: ?[]const u8, pos: ?Pos) Value {
        return Value{ .runtime_error = Error{ .msg = msg, .extra = extra, .pos = pos } };
    }

    pub fn add(allocator: std.mem.Allocator, lhs: Value, rhs: Value) Value {
        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = i + j },
                .float => |f| Value{ .float = @as(f64, @floatFromInt(i)) + f },
                else => Value.err("Invalid type", "Can't add nonnumeric values", null),
            },
            .float => |f| switch (rhs) {
                .integer => |i| Value{ .float = f + @as(f64, @floatFromInt(i)) },
                .float => |j| Value{ .float = f + j },
                else => Value.err("Invalid type", "Can't add nonnumeric values", null),
            },
            .string => |s| switch (rhs) {
                .string => |t| Value{ .string = s.concat(t) catch @panic("out of memory") },
                .char => |c| Value{ .string = s.concatRaw(&[_]u8{c}) catch @panic("out of memory") },
                else => Value.err("Invalid type", "Can't add string to nonstring", null),
            },
            .char => |c| switch (rhs) {
                .string => |s| Value{ .string = types.String.fromChar(c, allocator).concat(s) catch @panic("out of memory") },
                .char => |d| Value{ .string = types.String.fromConcatChars(c, d, allocator) },
                .integer, .float => blk: {
                    const char = safeIntCast(u8, rhs) catch break :blk Value.err("Invalid type", "Can't cast int or float to char (out of range 0-255)", null);
                    break :blk Value{ .string = types.String.fromConcatChars(c, char, allocator) };
                },
                else => Value.err("Invalid type", "Can't add string to nonstring", null),
            },
            .list => |l| switch (rhs) {
                .list => |r| blk: {
                    const new = types.List.init(allocator);
                    new.appendSlice(l.items) catch @panic("out of memory");
                    new.appendSlice(r.items) catch @panic("out of memory");
                    break :blk Value{ .list = new };
                },
                else => Value.err("Invalid type", "Can't add list to nonlist", null),
            },
            else => Value.err("Invalid type", "Can't add nonnumeric values", null),
        };
    }

    pub fn sub(lhs: Value, rhs: Value) Value {
        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;
        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = i - j },
                .float => |f| Value{ .float = @as(f64, @floatFromInt(i)) - f },
                else => Value.err("Invalid type", "Can't subtract nonnumeric values", null),
            },
            .float => |f| switch (rhs) {
                .integer => |i| Value{ .float = f - @as(f64, @floatFromInt(i)) },
                .float => |j| Value{ .float = f - j },
                else => Value.err("Invalid type", "Can't subtract nonnumeric values", null),
            },
            else => Value.err("Invalid type", "Can't subtract nonnumeric values", null),
        };
    }

    pub fn mul(allocator: std.mem.Allocator, lhs: Value, rhs: Value) Value {
        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;
        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = i * j },
                .float => |f| Value{ .float = @as(f64, @floatFromInt(i)) * f },
                else => Value.err("Invalid type", "Can't multiply nonnumeric values", null),
            },
            .float => |f| switch (rhs) {
                .integer => |i| Value{ .float = f * @as(f64, @floatFromInt(i)) },
                .float => |j| Value{ .float = f * j },
                else => Value.err("Invalid type", "Can't multiply nonnumeric values", null),
            },
            .string => |s| switch (rhs) {
                .integer => |i| Value{ .string = s.repeat(i) catch |e| switch (e) {
                    error.OutOfMemory => @panic("out of memory"),
                    error.NegativeRepeat => return Value.err("Invalid Repeat", "Can't repeat by a negative number", null),
                    else => unreachable,
                } },
                else => Value.err("Invalid type", "Can't multiply nonnumeric values", null),
            },
            .char => |c| switch (rhs) {
                .integer => |i| Value{ .string = types.String.initRepeat(allocator, c, i) catch |e| switch (e) {
                    error.OutOfMemory => @panic("out of memory"),
                    error.NegativeRepeat => return Value.err("Invalid Repeat", "Can't repeat by a negative number", null),
                    else => unreachable,
                } },
                else => Value.err("Invalid type", "Can't multiply nonnumeric values", null),
            },
            else => Value.err("Invalid type", "Can't multiply nonnumeric values", null),
        };
    }

    pub fn div(lhs: Value, rhs: Value) Value {
        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;
        switch (rhs) {
            .integer => |i| if (i == 0) return Value.err("Division by zero", "Can't divide by zero", null),
            .float => |f| if (f == 0) return Value.err("Division by zero", "Can't divide by zero", null),
            else => {},
        }

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = @divTrunc(i, j) },
                .float => |f| Value{ .float = @as(f64, @floatFromInt(i)) / f },
                else => Value.err("Invalid type", "Can't divide nonnumeric values", null),
            },
            .float => |f| switch (rhs) {
                .integer => |i| Value{ .float = f / @as(f64, @floatFromInt(i)) },
                .float => |j| Value{ .float = f / j },
                else => Value.err("Invalid type", "Can't divide nonnumeric values", null),
            },
            else => Value.err("Invalid type", "Can't divide nonnumeric values", null),
        };
    }

    pub fn mod(lhs: Value, rhs: Value) Value {
        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = @mod(i, j) },
                .float => |f| Value{ .float = @mod(@as(f64, @floatFromInt(i)), f) },
                else => Value.err("Invalid type", "Can't mod nonnumeric values", null),
            },
            .float => |f| switch (rhs) {
                .integer => |i| Value{ .float = @mod(f, @as(f64, @floatFromInt(i))) },
                .float => |j| Value{ .float = @mod(f, j) },
                else => Value.err("Invalid type", "Can't mod nonnumeric values", null),
            },
            else => Value.err("Invalid type", "Can't mod nonnumeric values", null),
        };
    }

    pub fn bitAnd(lhs: Value, rhs: Value) Value {
        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;
        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = i & j },
                .float => Value.err("Invalid type", "Can't bitwise and on float", null),
                else => Value.err("Invalid type", "Can't bitwise and nonnumeric values", null),
            },
            .float => Value.err("Invalid type", "Can't bitwise and on float", null),
            else => Value.err("Invalid type", "Can't bitwise and nonnumeric values", null),
        };
    }

    pub fn bitOr(lhs: Value, rhs: Value) Value {
        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;
        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = i | j },
                .float => Value.err("Invalid type", "Can't bitwise or on float", null),
                else => Value.err("Invalid type", "Can't bitwise or nonnumeric values", null),
            },
            .float => Value.err("Invalid type", "Can't bitwise or on float", null),
            else => Value.err("Invalid type", "Can't bitwise or nonnumeric values", null),
        };
    }

    pub fn bitXor(lhs: Value, rhs: Value) Value {
        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;
        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = i ^ j },
                .float => Value.err("Invalid type", "Can't bitwise xor on float", null),
                else => Value.err("Invalid type", "Can't bitwise xor nonnumeric values", null),
            },
            .float => Value.err("Invalid type", "Can't bitwise xor on float", null),
            else => Value.err("Invalid type", "Can't bitwise xor nonnumeric values", null),
        };
    }

    fn shiftCheck(rhs: Value) bool {
        const check2 = switch (rhs) {
            .integer => |i| i < @bitSizeOf(i65),
            else => true, // so we can handle checking that ourself
        };

        return check2;
    }

    const OpFunc = fn (lhs: Value, rhs: Value) Value;
    fn reverseShift(lhs: Value, rhs: Value, func: OpFunc) ?Value {
        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        if (!shiftCheck(rhs)) return Value.err("Invalid shift", "Shift value out of range", null);

        switch (rhs) {
            .integer => |i| if (i < 0) return func(lhs, Value{ .integer = -i }), // reverse shift neg to pos
            else => return null,
        }
        return null;
    }

    pub fn lshift(lhs: Value, rhs: Value) Value {
        if (reverseShift(lhs, rhs, rshift)) |v| return v;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = i << @truncate(@as(u65, @intCast(j))) },
                .float => Value.err("Invalid type", "Can't lshift on float", null),
                else => Value.err("Invalid type", "Can't lshift nonnumeric values", null),
            },
            .float => Value.err("Invalid type", "Can't lshift on float", null),
            else => Value.err("Invalid type", "Can't lshift nonnumeric values", null),
        };
    }

    pub fn rshift(lhs: Value, rhs: Value) Value {
        if (reverseShift(lhs, rhs, lshift)) |v| return v;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = i >> @truncate(@as(u65, @intCast(j))) },
                .float => Value.err("Invalid type", "Can't rshift on float", null),
                else => Value.err("Invalid type", "Can't rshift nonnumeric values", null),
            },
            .float => Value.err("Invalid type", "Can't rshift on float", null),
            else => Value.err("Invalid type", "Can't rshift nonnumeric values", null),
        };
    }

    // logical operators
    pub fn logAnd(lhs: Value, rhs: Value) Value {
        const lhsb = lhs.convertToBool(); // convert to bool or runtime_error
        if (lhsb == .runtime_error) return lhsb;
        if (!lhsb.boolean) return Value{ .boolean = false };

        return rhs.convertToBool();
    }

    pub fn logOr(lhs: Value, rhs: Value) Value {
        const lhsb = lhs.convertToBool(); // convert to bool or runtime_error
        if (lhsb == .runtime_error) return lhsb;
        if (lhsb.boolean) return Value{ .boolean = true };

        return rhs.convertToBool();
    }

    // this equal function is a bit different
    // it returns a bool instead of a Value use for comparisions outside of the runtime
    pub fn equalB(lhs: Value, rhs: Value) bool {
        if (lhs == .runtime_error) return false;
        if (rhs == .runtime_error) return false;

        switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| return i == j,
                .float => |f| return @as(f64, @floatFromInt(i)) == f,
                .boolean => |b| return b == (i != 0),
                .none => return i == 0,
                else => return false,
            },
            .float => |f| switch (rhs) {
                .integer => |i| return f == @as(f64, @floatFromInt(i)),
                .float => |j| return f == j,
                .boolean => |b| return b == (f != 0.0),
                .none => return f == 0.0,
                else => return false,
            },
            .boolean => |b| switch (rhs) {
                .integer => |i| return b == (i != 0),
                .float => |f| return b == (f != 0.0),
                .boolean => |j| return b == j,
                .none => return !b,
                else => return false,
            },
            .none => switch (rhs) {
                .integer => |i| return i == 0,
                .float => |f| return f == 0.0,
                .boolean => |b| return !b,
                .none => return true,
                else => return false,
            },
            .string => |s| switch (rhs) {
                .string => |j| return s.equal(j),
                .char => |c| return s.equalChar(c),
                .none => return s.value.len == 0,
                else => return false,
            },
            .char => |c| switch (rhs) {
                .char => |j| return c == j,
                .string => |s| return s.equalChar(c),
                .none => return c == 0,
                else => return false,
            },
            .list => |l| switch (rhs) {
                .list => |j| return l.equal(j),
                .none => return l.items.len == 0,
                else => return false,
            },
            else => return false,
        }
    }

    pub fn equal(lhs: Value, rhs: Value) Value {
        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .boolean = i == j },
                .float => |f| Value{ .boolean = @as(f64, @floatFromInt(i)) == f },
                .boolean => |b| Value{ .boolean = b == (i != 0) },
                .none => Value{ .boolean = i == 0 },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .float => |f| switch (rhs) {
                .integer => |i| Value{ .boolean = f == @as(f64, @floatFromInt(i)) },
                .float => |j| Value{ .boolean = f == j },
                .boolean => |b| Value{ .boolean = b == (f != 0.0) },
                .none => Value{ .boolean = f == 0.0 },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .boolean => |b| switch (rhs) {
                .integer => |i| Value{ .boolean = b == (i != 0) },
                .float => |f| Value{ .boolean = b == (f != 0.0) },
                .boolean => |j| Value{ .boolean = b == j },
                .char => |c| Value{ .boolean = b == (c != 0) },
                .string => |s| Value{ .boolean = b == (s.value.len > 0) },
                .none => Value{ .boolean = !b },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .string => |s| switch (rhs) {
                .string => |j| Value{ .boolean = s.value.len == j.value.len and std.mem.eql(u8, s.value, j.value) },
                .char => |c| Value{ .boolean = s.value.len == 1 and s.value[0] == c },
                .none => Value{ .boolean = s.value.len == 0 },
                else => Value.err("Invalid type", "Can't compare strings to nonstring values", null),
            },
            .char => |c| switch (rhs) {
                .char => |j| Value{ .boolean = c == j },
                .string => |s| Value{ .boolean = s.value.len == 1 and s.value[0] == c },
                .none => Value{ .boolean = c == 0 },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .none => switch (rhs) {
                .integer => |i| Value{ .boolean = i == 0 },
                .float => |f| Value{ .boolean = f == 0.0 },
                .boolean => |b| Value{ .boolean = false == b },
                .string => |s| Value{ .boolean = s.value.len == 0 },
                .char => |c| Value{ .boolean = c == 0 },
                .none => Value{ .boolean = true },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .func => Value.err("Invalid type", "Can't compare functions", null), // you can techincally compare functions but that's a bad idea hince the error
            .list => |l| switch (rhs) {
                .list => |j| Value{ .boolean = l.equal(j) },
                .none => Value{ .boolean = l.items.len == 0 },
                else => Value.err("Invalid type", "Can't compare list to nonlist values", null),
            },
            else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
        };
    }

    pub fn notEqual(lhs: Value, rhs: Value) Value {
        return equal(lhs, rhs).not();
    }

    pub fn less(lhs: Value, rhs: Value) Value {
        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;
        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .boolean = i < j },
                .float => |f| Value{ .boolean = @as(f64, @floatFromInt(i)) < f },
                .none => Value{ .boolean = i < 0 },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .float => |f| switch (rhs) {
                .integer => |i| Value{ .boolean = f < @as(f64, @floatFromInt(i)) },
                .float => |j| Value{ .boolean = f < j },
                .none => Value{ .boolean = f < 0.0 },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .none => switch (rhs) {
                .integer => |i| Value{ .boolean = 0 < i },
                .float => |f| Value{ .boolean = 0.0 < f },
                .none => Value{ .boolean = false },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
        };
    }

    pub fn greater(lhs: Value, rhs: Value) Value {
        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;
        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .boolean = i > j },
                .float => |f| Value{ .boolean = @as(f64, @floatFromInt(i)) > f },
                .none => Value{ .boolean = i > 0 },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .float => |f| switch (rhs) {
                .integer => |i| Value{ .boolean = f > @as(f64, @floatFromInt(i)) },
                .float => |j| Value{ .boolean = f > j },
                .none => Value{ .boolean = f > 0.0 },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .none => switch (rhs) {
                .integer => |i| Value{ .boolean = 0 > i },
                .float => |f| Value{ .boolean = 0.0 > f },
                .none => Value{ .boolean = false },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
        };
    }

    pub fn greaterEqual(lhs: Value, rhs: Value) Value {
        const gthen = greater(lhs, rhs);
        if (gthen == .runtime_error) return gthen;
        std.debug.assert(gthen == .boolean);
        if (gthen.boolean == true) return Value{ .boolean = true };

        const eql = equal(lhs, rhs);
        if (eql == .runtime_error) return eql;
        std.debug.assert(eql == .boolean);
        return Value{ .boolean = eql.boolean };
    }

    pub fn lessEqual(lhs: Value, rhs: Value) Value {
        const lthen = less(lhs, rhs);
        if (lthen == .runtime_error) return lthen;
        std.debug.assert(lthen == .boolean);
        if (lthen.boolean == true) return Value{ .boolean = true };

        const eql = equal(lhs, rhs);
        if (eql == .runtime_error) return eql;
        std.debug.assert(eql == .boolean);
        return Value{ .boolean = eql.boolean };
    }

    pub fn neg(self: Value) Value {
        if (self == .runtime_error) return self;
        return switch (self) {
            .integer => |i| Value{ .integer = -i },
            .float => |f| Value{ .float = -f },
            else => Value.err("Invalid type", "Can't negate nonnumeric values", null),
        };
    }

    pub fn not(self: Value) Value {
        const val = self.convertToBool(); // convert to bool or runtime_error
        if (val == .runtime_error) return val;
        std.debug.assert(val == .boolean);
        return Value{ .boolean = !val.boolean };
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        switch (self) {
            .integer => |i| try writer.print("{}", .{i}),
            .float => |f| try writer.print("{d}", .{f}),
            .boolean => |b| try writer.print("{}", .{b}),
            .char => |c| try writer.print("{c}", .{c}),
            .none => try writer.print("void", .{}),
            .runtime_error => |e| {
                try writer.print("Runtime error: {s}\n", .{e.msg});
                if (e.extra) |extra| {
                    try writer.print("{s}\n", .{extra});
                }
                if (e.pos) |pos| {
                    try writer.print("{}\n", .{pos});
                    try writer.print("line: {d}, column: {d}\n", .{ pos.line, pos.column });
                }
            },
            .string => |s| try writer.print("{s}", .{s.value}),
            .func => |f| try writer.print("[func {s}]", .{f.getName()}),
            .list => |l| {
                try writer.writeByte('[');
                for (l.items, 0..) |item, i| {
                    if (i != 0) try writer.writeByte(' ');
                    if (item == .string) try writer.writeByte('"');
                    if (item == .char) try writer.writeByte('\'');
                    try writer.print("{}", .{item});
                    if (item == .char) try writer.writeByte('\'');
                    if (item == .string) try writer.writeByte('"');
                    if (i != l.items.len - 1) try writer.writeByte(',');
                }
                try writer.writeByte(']');
            },
        }
    }
};

pub const Result = union(enum) {
    value: Value,
    signal: Signal,

    pub fn none() Result {
        return Result{
            .value = Value{ .none = {} },
        };
    }

    pub fn val(v: Value) Result {
        return Result{
            .value = v,
        };
    }

    pub fn err(msg: []const u8, extra: ?[]const u8, pos: ?Pos) Result {
        return Result{
            .value = Value.err(msg, extra, pos),
        };
    }

    pub fn errHeap(msg: []const u8, extra: []const u8, pos: ?Pos) Result {
        var rterr = Value.err(msg, extra, pos);
        rterr.runtime_error.extra_allocated = true;
        return Result{
            .value = rterr,
        };
    }

    pub fn sig(s: Signal) Result {
        return Result{
            .signal = s,
        };
    }
};

pub fn safeIntCast(comptime TO: type, v: Value) !TO {
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

pub fn safeFloatCast(comptime TO: type, v: Value) !TO {
    const max = std.math.floatMax(TO);
    const min = std.math.floatMin(TO);

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
    const new = v.convertToBool();
    if (new == .runtime_error) return error.InvalidCast;
    return new.boolean;
}

pub fn safeStrCast(allocator: std.mem.Allocator, v: Value) !types.String {
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

pub fn safeListCast(allocator: std.mem.Allocator, v: Value) !*types.List {
    return switch (v) {
        .string => |s| blk: {
            const new = types.List.init(allocator);
            new.resize(s.value.len) catch unreachable;
            for (s.value) |char| {
                try new.append(Value{ .char = char });
            }
            break :blk new;
        },
        .list => |l| l,
        else => return error.InvalidCast,
    };
}

pub fn castToSymbolValue(allocator: std.mem.Allocator, v: Value, ty: TypeVal) !SymbolTable.SymbolValue {
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
        .list => SymVal{ .list = try safeListCast(allocator, v) },
    };
}

// values are temporary and are not stored therefore we wanna clone them if needed
pub fn castToValue(v: SymbolTable.SymbolValue) Value {
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
        .string => |s| Value{ .string = s.clone() },
        .char => |c| Value{ .char = c },
        .func => |f| Value{ .func = f },
        .void => Value{ .none = {} },
        .list => |l| Value{ .list = l.clone() },
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

pub fn checkRuntimeError(value: Value, orgin: *const Node) ?Result {
    if (value == .runtime_error) {
        var err = value.runtime_error;
        if (err.pos == null) err.pos = orgin.getPos();
        return Result{ .value = .{ .runtime_error = err } };
    }
    return null;
}

// returns a runtime error if there is one
pub fn checkRuntimeErrorOrSignal(result: Result, orgin: *const Node) ?Result {
    switch (result) {
        .value => |v| if (v == .runtime_error) {
            var err = v.runtime_error;
            if (err.pos == null) err.pos = orgin.getPos();
            return Result{ .value = .{ .runtime_error = err } };
        },
        .signal => |s| return Result{ .signal = s },
    }
    return null;
}
