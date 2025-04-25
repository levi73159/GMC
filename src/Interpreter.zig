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

const Value = union(enum) {
    integer: i65,
    float: f64,
    boolean: bool,
    none,
    runtime_error: Error,

    // converts the value to a boolean value
    fn convertToBool(self: Value) Value {
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
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .none => switch (rhs) {
                .integer => |i| Value{ .boolean = i == 0 },
                .float => |f| Value{ .boolean = f == 0.0 },
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

const Self = @This();

symbols: *SymbolTable,

pub fn init(symbols: *SymbolTable) Self {
    return Self{ .symbols = symbols };
}

pub fn eval(self: Self, nodes: []const *Node) void {
    for (nodes) |node| {
        const result = self.evalNode(node);
        switch (result) {
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

pub fn evalNode(self: Self, node: *Node) Value {
    return switch (node.*) {
        .number => self.evalNumber(node.number),
        .boolean => Value{ .boolean = node.boolean.n },
        .bin_op => self.evalBinOp(node),
        .unary_op => self.evalUnaryOp(node),
        .var_decl => self.evalVarDecl(node),
        .var_assign => self.evalVarAssign(node),
        .identifier => self.evalIdentifier(node),
    };
}

fn evalNumber(_: Self, node: tree.Number) Value {
    return switch (node) {
        .integer => |i| Value{ .integer = i.n },
        .float => |f| Value{ .float = f.n },
    };
}

fn evalBinOp(self: Self, og_node: *Node) Value {
    const node = og_node.bin_op;
    var left = self.evalNode(node.left);
    if (left == .runtime_error) {
        if (left.runtime_error.pos == null) left.runtime_error.pos = node.left.getPos();
        return left;
    }

    // short circuiting operators
    switch (node.op.kind) {
        .ampersand_ampersand => {
            const left_bool = left.convertToBool();
            if (left_bool == .runtime_error) return left_bool;
            std.debug.assert(left_bool == .boolean);
            if (left_bool.boolean == false) return Value{ .boolean = false };
        },
        .pipe_pipe => {
            const left_bool = left.convertToBool();
            if (left_bool == .runtime_error) return left_bool;
            std.debug.assert(left_bool == .boolean);
            if (left_bool.boolean == true) return Value{ .boolean = true };
        },
        else => {},
    }

    var right = self.evalNode(node.right);
    if (right == .runtime_error) {
        if (right.runtime_error.pos == null) right.runtime_error.pos = node.right.getPos();
        return right;
    }

    var result = switch (node.op.kind) {
        .plus => Value.add(left, right),
        .minus => Value.sub(left, right),
        .star => Value.mul(left, right),
        .slash => Value.div(left, right),
        .percent => Value.mod(left, right),
        .ampersand => Value.bitAnd(left, right),
        .pipe => Value.bitOr(left, right),
        .caret => Value.bitXor(left, right),
        .lt_lt => Value.lshift(left, right),
        .gt_gt => Value.rshift(left, right),
        .equal_equal => Value.equal(left, right),
        .bang_equal => Value.notEqual(left, right),
        .lt => Value.less(left, right),
        .lt_equal => Value.lessEqual(left, right),
        .gt => Value.greater(left, right),
        .gt_equal => Value.greaterEqual(left, right),
        .ampersand_ampersand => Value.logAnd(left, right),
        .pipe_pipe => Value.logOr(left, right),
        else => Value.err("Invalid operator", "Invalid Binary Operator", node.op.pos),
    };

    if (result == .runtime_error) {
        if (result.runtime_error.pos == null) result.runtime_error.pos = og_node.getPos();
    }

    return result;
}

fn evalUnaryOp(self: Self, og_node: *Node) Value {
    const node = og_node.unary_op;
    var right = self.evalNode(node.right);
    if (right == .runtime_error) {
        if (right.runtime_error.pos == null) right.runtime_error.pos = node.right.getPos();
        return right;
    }

    var result = switch (node.op.kind) {
        .minus => Value.neg(right),
        .plus => right, // ignore + just in case it does not in the Parser
        .bang => Value.not(right),
        else => Value.err("Invalid operator", "Invalid Unary Operator", node.op.pos),
    };

    if (result == .runtime_error) {
        if (result.runtime_error.pos == null) result.runtime_error.pos = og_node.getPos();
    }

    return result;
}

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
        .null => Value.none,

        .bool => |b| Value{ .boolean = b },
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

        .null => return error.InvalidCast,
    };
}

fn evalVarDecl(self: Self, og_node: *Node) Value {
    const node = og_node.var_decl;
    var value: Value = if (node.value) |v| self.evalNode(v) else .none;
    if (value == .runtime_error) {
        if (value.runtime_error.pos == null) value.runtime_error.pos = og_node.getPos();
        return value;
    }

    std.debug.assert(node.type.value == .type);

    const symval = castToSymbolValue(value, node.type.value.type) catch {
        return Value.err("Invalid Cast", "The value can't be converted to the type (could be due to the value is too big or small)", (node.value orelse og_node).getPos());
    };
    const symbol = SymbolTable.Symbol{ .is_const = node.is_const.n, .value = symval };

    self.symbols.add(node.identifier.lexeme, symbol) catch |err| switch (err) {
        error.OutOfMemory => std.debug.panic("OUT OF MEMORY!!!", .{}),
        error.SymbolAlreadyExists => return Value.err("Symbol already exists", "The symbol already exists", node.identifier.pos),
    };

    return value;
}

fn evalVarAssign(self: Self, og_node: *Node) Value {
    const node = og_node.var_assign;
    var value = self.evalNode(node.value);
    if (value == .runtime_error) {
        if (value.runtime_error.pos == null) value.runtime_error.pos = og_node.getPos();
        return value;
    }

    const old_symbol = self.symbols.get(node.identifier.lexeme) orelse {
        return Value.err("Symbol not found", "The symbol was not found", node.identifier.pos);
    };
    const ty = getTypeValFromSymbolValue(old_symbol.value) catch {
        return Value.err("Invalid Cast", "Can't convert the value into the type", og_node.getPos());
    };
    const symval = castToSymbolValue(value, ty) catch {
        return Value.err("Invalid Cast", "The value can't be converted to the type (could be due to the value is too big or small)", node.value.getPos());
    };

    self.symbols.set(node.identifier.lexeme, symval) catch |err| switch (err) {
        error.SymbolDoesNotExist => return Value.err("Symbol not found", "The symbol given was not found", node.identifier.pos),
        error.SymbolIsImmutable => return Value.err("Symbol is immutable", "The symbol is immutable (const)", og_node.getPos()),
        error.InvalidTypes => return Value.err("Invalid Types", "Can't change a symbol's type", og_node.getPos()),
    };
    return castToValue(symval);
}

fn evalIdentifier(self: Self, og_node: *Node) Value {
    const token = og_node.identifier;
    const symbol = self.symbols.get(token.lexeme) orelse {
        return Value.err("Symbol not found", "The symbol was not found", token.pos);
    };
    return castToValue(symbol.value);
}
