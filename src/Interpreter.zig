const std = @import("std");

const Pos = @import("DebugPos.zig");
const SymbolTable = @import("SymbolTable.zig");
const tree = @import("tree.zig");
const Node = tree.Node;
const TypeVal = @import("Token.zig").TypeValue;

const rt = @import("runtime.zig");

// common runtime functions for easy access
const checkRuntimeError = rt.checkRuntimeError;
const checkRuntimeErrorOrSignal = rt.checkRuntimeErrorOrSignal;

const Self = @This();

allocator: std.mem.Allocator,
symbols: *SymbolTable,
heap_str_only: bool = false,

pub fn init(allocator: std.mem.Allocator, symbols: *SymbolTable) Self {
    return Self{ .allocator = allocator, .symbols = symbols };
}

pub fn eval(self: Self, nodes: []const *Node) void {
    for (nodes) |node| {
        var result = self.evalNode(node);
        if (result == .signal) {
            result = rt.Result.err("Signal error", "Can't handle signal at current scope", node.getPos());
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
            .string => |s| {
                std.debug.print("Result: {s}\n", .{s.value});
            },
            .char => |c| std.debug.print("Result: {c}\n", .{c}),
            .none => std.debug.print("Result: None\n", .{}),
        }
        result.value.deinit();
    }
}

pub fn evalNode(self: Self, node: *Node) rt.Result {
    return switch (node.*) {
        .number => self.evalNumber(node.number),
        .string => self.evalString(node),
        .char => self.evalCharacter(node),
        .boolean => rt.Result.val(rt.Value{ .boolean = node.boolean.n }),
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
        .continuestmt => rt.Result.sig(.@"continue"),
    };
}

fn evalBlock(self: Self, og_node: *Node) rt.Result {
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    defer arena.deinit();

    const block_allocator = arena.allocator();
    var symbols = SymbolTable.init(block_allocator);
    defer symbols.deinit();

    symbols.parent = self.symbols;
    var interpreter = Self{ .symbols = &symbols, .allocator = block_allocator };

    var last_value: rt.Result = rt.Result.none();
    for (og_node.block.nodes) |node| {
        last_value = interpreter.evalNode(node);
        if (checkRuntimeErrorOrSignal(last_value, node)) |sigOrErr| return sigOrErr;
    }
    return last_value;
}

fn evalNumber(_: Self, node: tree.Number) rt.Result {
    return rt.Result.val(switch (node) {
        .integer => |i| rt.Value{ .integer = i.n },
        .float => |f| rt.Value{ .float = f.n },
    });
}

fn evalBinOp(self: Self, og_node: *Node) rt.Result {
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
            if (left_bool.boolean == false) return rt.Result.val(rt.Value{ .boolean = false });
        },
        .pipe_pipe => {
            const left_bool = val_left.convertToBool();
            if (checkRuntimeError(left_bool, node.left)) |err| return err;
            std.debug.assert(left_bool == .boolean);
            if (left_bool.boolean == true) return rt.Result.val(rt.Value{ .boolean = true });
        },
        else => {},
    }

    const right = self.evalNode(node.right);
    if (checkRuntimeErrorOrSignal(right, node.right)) |err| return err;
    const val_right = right.value;

    const result = switch (node.op.kind) {
        .plus => rt.Value.add(self.allocator, val_left, val_right),
        .minus => rt.Value.sub(val_left, val_right),
        .star => rt.Value.mul(self.allocator, val_left, val_right),
        .slash => rt.Value.div(val_left, val_right),
        .percent => rt.Value.mod(val_left, val_right),
        .ampersand => rt.Value.bitAnd(val_left, val_right),
        .pipe => rt.Value.bitOr(val_left, val_right),
        .caret => rt.Value.bitXor(val_left, val_right),
        .lt_lt => rt.Value.lshift(val_left, val_right),
        .gt_gt => rt.Value.rshift(val_left, val_right),
        .equal_equal => rt.Value.equal(val_left, val_right),
        .bang_equal => rt.Value.notEqual(val_left, val_right),
        .lt => rt.Value.less(val_left, val_right),
        .lt_equal => rt.Value.lessEqual(val_left, val_right),
        .gt => rt.Value.greater(val_left, val_right),
        .gt_equal => rt.Value.greaterEqual(val_left, val_right),
        .ampersand_ampersand => rt.Value.logAnd(val_left, val_right),
        .pipe_pipe => rt.Value.logOr(val_left, val_right),
        else => rt.Value.err("Invalid operator", "Invalid Binary Operator", node.op.pos),
    };

    if (checkRuntimeError(result, og_node)) |err| return err;
    return rt.Result.val(result);
}

fn evalUnaryOp(self: Self, og_node: *Node) rt.Result {
    const node = og_node.unary_op;
    const right: rt.Result = self.evalNode(node.right);
    if (checkRuntimeErrorOrSignal(right, node.right)) |err| return err;
    const val_right: rt.Value = right.value;

    const result: rt.Value = switch (node.op.kind) {
        .minus => rt.Value.neg(val_right),
        .plus => val_right, // ignore + just in case it does not in the Parser
        .bang => rt.Value.not(val_right),
        else => rt.Value.err("Invalid operator", "Invalid Unary Operator", node.op.pos),
    };

    if (checkRuntimeError(result, og_node)) |err| return err;

    return rt.Result.val(result);
}

fn evalVarDecl(self: Self, og_node: *Node) rt.Result {
    const node = og_node.var_decl;
    const value: rt.Value = if (node.value) |v| blk: {
        const result = self.evalNode(v);
        if (checkRuntimeErrorOrSignal(result, v)) |err| return err;
        break :blk result.value;
    } else .none;

    if (checkRuntimeError(value, node.value orelse og_node)) |err| return err;
    std.debug.assert(node.type.value == .type);

    const symval = rt.castToSymbolValue(self.allocator, value.clone(), node.type.value.type) catch {
        return rt.Result.err("Invalid Cast", "The value can't be converted to the type (could be due to the value is too big or small)", (node.value orelse og_node).getPos());
    };
    const symbol = SymbolTable.Symbol{ .is_const = node.is_const.n, .value = symval };

    self.symbols.add(node.identifier.lexeme, symbol) catch |err| switch (err) {
        error.OutOfMemory => std.debug.panic("OUT OF MEMORY!!!", .{}),
        error.SymbolAlreadyExists => return rt.Result.err("Symbol already exists", "The symbol already exists", node.identifier.pos),
    };

    return rt.Result.val(value);
}

fn evalVarAssign(self: Self, og_node: *Node) rt.Result {
    const node = og_node.var_assign;
    const rtresult = self.evalNode(node.value);
    if (checkRuntimeErrorOrSignal(rtresult, node.value)) |err| return err;
    const value = rtresult.value;

    const old_symbol = self.symbols.get(node.identifier.lexeme) orelse {
        return rt.Result.err("Symbol not found", "The symbol was not found", node.identifier.pos);
    };
    const ty = rt.getTypeValFromSymbolValue(old_symbol.value) catch {
        return rt.Result.err("Invalid Cast", "Can't convert the value into the type", og_node.getPos());
    };
    const symval = rt.castToSymbolValue(self.allocator, value, ty) catch {
        return rt.Result.err("Invalid Cast", "The value can't be converted to the type (could be due to the value is too big or small)", node.value.getPos());
    };

    self.symbols.set(node.identifier.lexeme, symval) catch |err| switch (err) {
        error.SymbolDoesNotExist => return rt.Result.err("Symbol not found", "The symbol given was not found", node.identifier.pos),
        error.SymbolIsImmutable => return rt.Result.err("Symbol is immutable", "The symbol is immutable (const)", og_node.getPos()),
        error.InvalidTypes => return rt.Result.err("Invalid Types", "Can't change a symbol's type", og_node.getPos()),
    };
    return rt.Result.val(rt.castToValue(symval));
}

fn evalIfStmt(self: Self, og_node: *Node) rt.Result {
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
    return rt.Result.none();
}

fn evalForStmt(self: Self, og_node: *Node) rt.Result {
    const node = og_node.forstmt;
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    defer arena.deinit();

    var symbols = SymbolTable.init(arena.allocator());
    symbols.parent = self.symbols;
    defer symbols.deinit();

    const outer_scope = Self{ .symbols = &symbols, .allocator = self.allocator };
    const start = outer_scope.evalNode(node.start_statement); // evaluate start node
    if (checkRuntimeErrorOrSignal(start, node.start_statement)) |err| return err;

    while (true) {
        const condition = outer_scope.evalNode(node.condition);
        if (checkRuntimeErrorOrSignal(condition, node.condition)) |err| return err; // if we get a signal like continue or break ignore it in the condition
        const val_cond = condition.value;

        const boolean_value = val_cond.convertToBool();
        if (checkRuntimeError(boolean_value, node.condition)) |err| return err;

        if (!boolean_value.boolean) break; // break out of loop

        var inner_arena = std.heap.ArenaAllocator.init(self.allocator);
        defer inner_arena.deinit();

        var inner_symbols = SymbolTable.init(inner_arena.allocator());
        inner_symbols.parent = &symbols;
        defer inner_symbols.deinit();

        const inner_scope = Self{ .symbols = &inner_symbols, .allocator = self.allocator };

        const body = inner_scope.evalNode(node.body);
        if (checkRuntimeErrorOrSignal(body, node.body)) |sigOrErr| switch (sigOrErr) {
            .signal => |signal| switch (signal) {
                .@"break" => |v| return rt.Result.val(v),
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

    return rt.Result.none();
}

fn evalWhileStmt(self: Self, og_node: *Node) rt.Result {
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
                .@"break" => |v| return rt.Result.val(v),
                .@"continue" => {},
            },
            else => return sigOrErr,
        };
    }

    return rt.Result.none();
}

fn evalIdentifier(self: Self, og_node: *Node) rt.Result {
    const token = og_node.identifier;
    const symbol = self.symbols.get(token.lexeme) orelse {
        return rt.Result.err("Symbol not found", "The symbol was not found", token.pos);
    };
    return rt.Result.val(rt.castToValue(symbol.value));
}

fn evalBreak(self: Self, og_node: *Node) rt.Result {
    const node = og_node.breakstmt;
    if (node.value) |val_node| {
        const result = self.evalNode(val_node);
        if (checkRuntimeErrorOrSignal(result, og_node)) |err| return err;
        const value = result.value;

        return rt.Result.sig(.{ .@"break" = value });
    } else {
        return rt.Result.sig(.{ .@"break" = .none });
    }
}

fn evalString(self: Self, og_node: *Node) rt.Result {
    const node = og_node.string;
    return rt.Result.val(rt.Value.str(node, self.heap_str_only, self.allocator) catch @panic("OUT OF MEMORY"));
}

fn evalCharacter(self: Self, og_node: *Node) rt.Result {
    _ = self;
    const node = og_node.char;
    return rt.Result.val(.{ .char = node.n });
}
