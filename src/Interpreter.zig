const std = @import("std");

const tree = @import("tree.zig");
const Node = tree.Node;
const Pos = @import("DebugPos.zig");
const SymbolTable = @import("SymbolTable.zig");
const TypeVal = @import("Token.zig").TypeValue;

const Error = struct {
    msg: []const u8,
    extra: ?[]const u8,
    pos: ?Pos,
};

const Signal = union(enum) {
    @"break": Value,
    @"continue": void,
};

const Value = union(enum) {
    integer: i65,
    float: f64,
    boolean: bool,
    none,
    runtime_error: Error,

    // converts the value to a boolean value
    pub fn convertToBool(self: Value) Value {
        return switch (self) {
            .integer => |i| Value{ .boolean = i != 0 },
            .float => |f| Value{ .boolean = f != 0.0 },
            .none => Value{ .boolean = false }, // none == false
            .boolean, .runtime_error => self,
        };
    }

    pub fn err(msg: []const u8, extra: []const u8, pos: ?Pos) Value {
        return Value{ .runtime_error = Error{ .msg = msg, .extra = extra, .pos = pos } };
    }

    pub fn add(lhs: Value, rhs: Value) Value {
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

    pub fn mul(lhs: Value, rhs: Value) Value {
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
                .none => Value{ .boolean = !b },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .none => switch (rhs) {
                .integer => |i| Value{ .boolean = i == 0 },
                .float => |f| Value{ .boolean = f == 0.0 },
                .boolean => |b| Value{ .boolean = false == b },
                .none => Value{ .boolean = true },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
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
};

const RTResult = union(enum) {
    value: Value,
    signal: Signal,

    pub fn none() RTResult {
        return RTResult{
            .value = Value{ .none = {} },
        };
    }

    pub fn val(v: Value) RTResult {
        return RTResult{
            .value = v,
        };
    }

    pub fn err(msg: []const u8, extra: []const u8, pos: ?Pos) RTResult {
        return RTResult{
            .value = Value.err(msg, extra, pos),
        };
    }

    pub fn sig(s: Signal) RTResult {
        return RTResult{
            .signal = s,
        };
    }
};

const Self = @This();

symbols: *SymbolTable,

fn safeIntCast(comptime TO: type, v: Value) !TO {
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
        .none => return 0, // cast none types to 0
        else => return error.InvalidCast,
    }
}

fn safeFloatCast(comptime TO: type, v: Value) !TO {
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
        .none => return 0.0, // cast none types to 0
        else => return error.InvalidCast,
    }
}

fn safeBoolCast(v: Value) !bool {
    const new = v.convertToBool();
    if (new == .runtime_error) return error.InvalidCast;
    return new.boolean;
}

fn castToSymbolValue(v: Value, ty: TypeVal) !SymbolTable.SymbolValue {
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

        .f32 => SymVal{ .f32 = try safeFloatCast(f32, v) },
        .f64 => SymVal{ .f64 = try safeFloatCast(f64, v) },

        .bool => SymVal{ .bool = try safeBoolCast(v) },
        .void => SymVal{ .void = {} },
    };
}

fn castToValue(v: SymbolTable.SymbolValue) Value {
    return switch (v) {
        .i8 => |i| Value{ .integer = i },
        .i16 => |i| Value{ .integer = i },
        .i32 => |i| Value{ .integer = i },
        .i64 => |i| Value{ .integer = i },
        .u8 => |i| Value{ .integer = i },
        .u16 => |i| Value{ .integer = i },
        .u32 => |i| Value{ .integer = i },
        .u64 => |i| Value{ .integer = i },

        .f32 => |f| Value{ .float = f },
        .f64 => |f| Value{ .float = f },

        .bool => |b| Value{ .boolean = b },
        .void => Value{ .none = {} },
    };
}

fn getTypeValFromSymbolValue(v: SymbolTable.SymbolValue) !TypeVal {
    return switch (v) {
        .i8 => TypeVal.i8,
        .i16 => TypeVal.i16,
        .i32 => TypeVal.i32,
        .i64 => TypeVal.i64,
        .u8 => TypeVal.u8,
        .u16 => TypeVal.u16,
        .u32 => TypeVal.u32,
        .u64 => TypeVal.u64,

        .f32 => TypeVal.f32,
        .f64 => TypeVal.f64,

        .bool => TypeVal.bool,
        .void => TypeVal.void,
    };
}

fn checkRuntimeError(value: Value, orgin: *Node) ?RTResult {
    if (value == .runtime_error) {
        var err = value.runtime_error;
        if (err.pos == null) err.pos = orgin.getPos();
        return RTResult{ .value = value };
    }
    return null;
}

// returns a runtime error if there is one
fn checkRuntimeErrorOrSignal(result: RTResult, orgin: *Node) ?RTResult {
    switch (result) {
        .value => |v| if (v == .runtime_error) {
            var err = v.runtime_error;
            if (err.pos == null) err.pos = orgin.getPos();
            return RTResult{ .value = v };
        },
        .signal => |s| return RTResult{ .signal = s },
    }
    return null;
}

pub fn init(symbols: *SymbolTable) Self {
    return Self{ .symbols = symbols };
}

pub fn eval(self: Self, nodes: []const *Node) void {
    for (nodes) |node| {
        var result = self.evalNode(node);
        if (result == .signal) {
            result = RTResult.err("Signal error", "Can't handle signal at current scope", node.getPos());
        }
        switch (result.value) {
            .runtime_error => |err| {
                std.debug.print("Runtime error: {s}\n", .{err.msg});
                if (err.extra) |extra| {
                    std.debug.print("{s}\n", .{extra});
                }
                if (err.pos) |pos| {
                    std.debug.print("{}\n", .{pos});
                    std.debug.print("line: {d}, column: {d}\n", .{ pos.line, pos.column });
                }
                return; // if error in program return instantly
            },
            .integer => |i| std.debug.print("Result: {}\n", .{i}),
            .float => |f| std.debug.print("Result: {d}\n", .{f}),
            .boolean => |b| std.debug.print("Result: {}\n", .{b}),
            .none => std.debug.print("Result: None\n", .{}),
        }
    }
}

pub fn evalNode(self: Self, node: *Node) RTResult {
    return switch (node.*) {
        .number => self.evalNumber(node.number),
        .boolean => RTResult.val(Value{ .boolean = node.boolean.n }),
        .block => self.evalBlock(node),
        .bin_op => self.evalBinOp(node),
        .unary_op => self.evalUnaryOp(node),
        .var_decl => self.evalVarDecl(node),
        .var_assign => self.evalVarAssign(node),
        .identifier => self.evalIdentifier(node),
        .ifstmt => self.evalIfStmt(node),
        .forstmt => self.evalForStmt(node),
        .whilestmt => self.evalWhileStmt(node),
        .breakstmt => self.evalBreak(node),
        .continuestmt => RTResult.sig(.@"continue"),
    };
}

fn evalBlock(self: Self, og_node: *Node) RTResult {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const block_allocator = arena.allocator();
    var symbols = SymbolTable.init(block_allocator);
    defer symbols.deinit();

    symbols.parent = self.symbols;
    var interpreter = Self{ .symbols = &symbols };

    var last_value: RTResult = RTResult.none();
    for (og_node.block.nodes) |node| {
        last_value = interpreter.evalNode(node);
        if (checkRuntimeErrorOrSignal(last_value, node)) |sigOrErr| return sigOrErr;
    }
    return last_value;
}

fn evalNumber(_: Self, node: tree.Number) RTResult {
    return RTResult.val(switch (node) {
        .integer => |i| Value{ .integer = i.n },
        .float => |f| Value{ .float = f.n },
    });
}

fn evalBinOp(self: Self, og_node: *Node) RTResult {
    const node = og_node.bin_op;
    const left = self.evalNode(node.left);
    if (checkRuntimeErrorOrSignal(left, node.left)) |err| return err;
    const val_left = left.value;

    // short circuiting operators
    switch (node.op.kind) {
        .ampersand_ampersand => {
            const left_bool = val_left.convertToBool();
            if (checkRuntimeError(left_bool, node.left)) |err| return err;
            std.debug.assert(left_bool == .boolean);
            if (left_bool.boolean == false) return RTResult.val(Value{ .boolean = false });
        },
        .pipe_pipe => {
            const left_bool = val_left.convertToBool();
            if (checkRuntimeError(left_bool, node.left)) |err| return err;
            std.debug.assert(left_bool == .boolean);
            if (left_bool.boolean == true) return RTResult.val(Value{ .boolean = true });
        },
        else => {},
    }

    const right = self.evalNode(node.right);
    if (checkRuntimeErrorOrSignal(right, node.right)) |err| return err;
    const val_right = right.value;

    const result = switch (node.op.kind) {
        .plus => Value.add(val_left, val_right),
        .minus => Value.sub(val_left, val_right),
        .star => Value.mul(val_left, val_right),
        .slash => Value.div(val_left, val_right),
        .percent => Value.mod(val_left, val_right),
        .ampersand => Value.bitAnd(val_left, val_right),
        .pipe => Value.bitOr(val_left, val_right),
        .caret => Value.bitXor(val_left, val_right),
        .lt_lt => Value.lshift(val_left, val_right),
        .gt_gt => Value.rshift(val_left, val_right),
        .equal_equal => Value.equal(val_left, val_right),
        .bang_equal => Value.notEqual(val_left, val_right),
        .lt => Value.less(val_left, val_right),
        .lt_equal => Value.lessEqual(val_left, val_right),
        .gt => Value.greater(val_left, val_right),
        .gt_equal => Value.greaterEqual(val_left, val_right),
        .ampersand_ampersand => Value.logAnd(val_left, val_right),
        .pipe_pipe => Value.logOr(val_left, val_right),
        else => Value.err("Invalid operator", "Invalid Binary Operator", node.op.pos),
    };

    if (checkRuntimeError(result, og_node)) |err| return err;
    return RTResult.val(result);
}

fn evalUnaryOp(self: Self, og_node: *Node) RTResult {
    const node = og_node.unary_op;
    const right: RTResult = self.evalNode(node.right);
    if (checkRuntimeErrorOrSignal(right, node.right)) |err| return err;
    const val_right: Value = right.value;

    const result: Value = switch (node.op.kind) {
        .minus => Value.neg(val_right),
        .plus => val_right, // ignore + just in case it does not in the Parser
        .bang => Value.not(val_right),
        else => Value.err("Invalid operator", "Invalid Unary Operator", node.op.pos),
    };

    if (checkRuntimeError(result, og_node)) |err| return err;

    return RTResult.val(result);
}

fn evalVarDecl(self: Self, og_node: *Node) RTResult {
    const node = og_node.var_decl;
    const value: Value = if (node.value) |v| blk: {
        const result = self.evalNode(v);
        if (checkRuntimeErrorOrSignal(result, v)) |err| return err;
        break :blk result.value;
    } else .none;

    if (checkRuntimeError(value, node.value orelse og_node)) |err| return err;
    std.debug.assert(node.type.value == .type);

    const symval = castToSymbolValue(value, node.type.value.type) catch {
        return RTResult.err("Invalid Cast", "The value can't be converted to the type (could be due to the value is too big or small)", (node.value orelse og_node).getPos());
    };
    const symbol = SymbolTable.Symbol{ .is_const = node.is_const.n, .value = symval };

    self.symbols.add(node.identifier.lexeme, symbol) catch |err| switch (err) {
        error.OutOfMemory => std.debug.panic("OUT OF MEMORY!!!", .{}),
        error.SymbolAlreadyExists => return RTResult.err("Symbol already exists", "The symbol already exists", node.identifier.pos),
    };

    return RTResult.val(value);
}

fn evalVarAssign(self: Self, og_node: *Node) RTResult {
    const node = og_node.var_assign;
    const rtresult = self.evalNode(node.value);
    if (checkRuntimeErrorOrSignal(rtresult, node.value)) |err| return err;
    const value = rtresult.value;

    const old_symbol = self.symbols.get(node.identifier.lexeme) orelse {
        return RTResult.err("Symbol not found", "The symbol was not found", node.identifier.pos);
    };
    const ty = getTypeValFromSymbolValue(old_symbol.value) catch {
        return RTResult.err("Invalid Cast", "Can't convert the value into the type", og_node.getPos());
    };
    const symval = castToSymbolValue(value, ty) catch {
        return RTResult.err("Invalid Cast", "The value can't be converted to the type (could be due to the value is too big or small)", node.value.getPos());
    };

    self.symbols.set(node.identifier.lexeme, symval) catch |err| switch (err) {
        error.SymbolDoesNotExist => return RTResult.err("Symbol not found", "The symbol given was not found", node.identifier.pos),
        error.SymbolIsImmutable => return RTResult.err("Symbol is immutable", "The symbol is immutable (const)", og_node.getPos()),
        error.InvalidTypes => return RTResult.err("Invalid Types", "Can't change a symbol's type", og_node.getPos()),
    };
    return RTResult.val(castToValue(symval));
}

fn evalIfStmt(self: Self, og_node: *Node) RTResult {
    const node = og_node.ifstmt;
    const condition = self.evalNode(node.condition);
    if (checkRuntimeErrorOrSignal(condition, node.condition)) |err| return err;
    const val_cond = condition.value;

    const boolean_value = val_cond.convertToBool();
    if (checkRuntimeError(boolean_value, node.condition)) |err| return err;
    std.debug.assert(boolean_value == .boolean); // should always be bool but just a safety check

    if (boolean_value.boolean) {
        const then = self.evalNode(node.then);
        if (checkRuntimeErrorOrSignal(then, node.then)) |err| return err;
        return then;
    } else {
        if (node.else_node) |else_node| {
            const else_value = self.evalNode(else_node);
            if (checkRuntimeErrorOrSignal(else_value, else_node)) |err| return err;
            return else_value;
        }
    }
    return RTResult.none();
}

fn evalForStmt(self: Self, og_node: *Node) RTResult {
    const node = og_node.forstmt;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var symbols = SymbolTable.init(arena.allocator());
    symbols.parent = self.symbols;
    defer symbols.deinit();

    const outer_scope = Self{ .symbols = &symbols };
    const start = outer_scope.evalNode(node.start_statement); // evaluate start node
    if (checkRuntimeErrorOrSignal(start, node.start_statement)) |err| return err;

    while (true) {
        const condition = outer_scope.evalNode(node.condition);
        if (checkRuntimeErrorOrSignal(condition, node.condition)) |err| return err; // if we get a signal like continue or break ignore it in the condition
        const val_cond = condition.value;

        const boolean_value = val_cond.convertToBool();
        if (checkRuntimeError(boolean_value, node.condition)) |err| return err;

        if (!boolean_value.boolean) break; // break out of loop

        var inner_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer inner_arena.deinit();

        var inner_symbols = SymbolTable.init(inner_arena.allocator());
        inner_symbols.parent = &symbols;
        defer inner_symbols.deinit();

        const inner_scope = Self{ .symbols = &inner_symbols };

        const body = inner_scope.evalNode(node.body);
        if (checkRuntimeErrorOrSignal(body, node.body)) |sigOrErr| switch (sigOrErr) {
            .signal => |signal| switch (signal) {
                .@"break" => |v| return RTResult.val(v),
                .@"continue" => {},
            },
            else => return sigOrErr,
        };

        const every_iteration = outer_scope.evalNode(node.every_iteration);
        if (checkRuntimeErrorOrSignal(every_iteration, node.every_iteration)) |err| return err;
    }

    if (node.else_node) |else_node| {
        const else_value = self.evalNode(else_node);
        if (checkRuntimeErrorOrSignal(else_value, else_node)) |err| return err;
        return else_value;
    }

    return RTResult.none();
}

fn evalWhileStmt(self: Self, og_node: *Node) RTResult {
    const node = og_node.whilestmt;
    while (true) {
        const condition = self.evalNode(node.condition);
        if (checkRuntimeErrorOrSignal(condition, node.condition)) |err| return err; // if we get a signal like continue or break ignore it in the condition
        const val_cond = condition.value;

        const boolean_value = val_cond.convertToBool();
        if (checkRuntimeError(boolean_value, node.condition)) |err| return err;

        if (!boolean_value.boolean) break; // break out of loop

        const body = self.evalNode(node.body);
        if (checkRuntimeErrorOrSignal(body, node.body)) |sigOrErr| switch (sigOrErr) {
            .signal => |signal| switch (signal) {
                .@"break" => |v| return RTResult.val(v),
                .@"continue" => {},
            },
            else => return sigOrErr,
        };
    }

    return RTResult.none();
}

fn evalIdentifier(self: Self, og_node: *Node) RTResult {
    const token = og_node.identifier;
    const symbol = self.symbols.get(token.lexeme) orelse {
        return RTResult.err("Symbol not found", "The symbol was not found", token.pos);
    };
    return RTResult.val(castToValue(symbol.value));
}

fn evalBreak(self: Self, og_node: *Node) RTResult {
    const node = og_node.breakstmt;
    if (node.value) |val_node| {
        const result = self.evalNode(val_node);
        if (checkRuntimeErrorOrSignal(result, og_node)) |err| return err;
        const value = result.value;

        return RTResult.sig(.{ .@"break" = value });
    } else {
        return RTResult.sig(.{ .@"break" = .none });
    }
}
