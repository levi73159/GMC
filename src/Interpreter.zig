const std = @import("std");

const Pos = @import("DebugPos.zig");
const SymbolTable = @import("SymbolTable.zig");
const tree = @import("tree.zig");
const Node = tree.Node;
const TypeVal = @import("Token.zig").TypeValue;
const Type = @import("Type.zig");

const rt = @import("runtime.zig");
const ty = @import("types.zig");

// common runtime functions for easy access
const checkRuntimeError = rt.checkRuntimeError;
const checkRuntimeErrorOrSignal = rt.checkRuntimeErrorOrSignal;

const Self = @This();

allocator: std.mem.Allocator,
symbols: *SymbolTable,
heap_str_only: bool = false,
static: bool = false,

static_checking: bool = true,

pub fn init(allocator: std.mem.Allocator, symbols: *SymbolTable) Self {
    return Self{
        .allocator = allocator,
        .symbols = symbols,
    };
}

pub fn newScope(self: Self, symbols: *SymbolTable, allocator: std.mem.Allocator) Self {
    return Self{
        .allocator = allocator,
        .static = self.static,
        .symbols = symbols,
        .heap_str_only = self.heap_str_only,
    };
}

pub fn eval(self: Self, nodes: []const *const Node) bool {
    for (nodes) |node| {
        var result = self.evalNode(node);
        if (result == .signal) {
            result = rt.Result.err("Signal error", "Can't handle signal at current scope", node.getPos());
        }
        defer result.value.deinit();
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
                return false; // if error in program return instantly
            },
            else => {},
        }
    }
    return true;
}

pub fn evalResult(self: Self, nodes: []const *const Node) rt.Result {
    var last_value: rt.Result = rt.Result.none();
    for (nodes) |node| {
        last_value = self.evalNode(node);
        if (last_value == .signal) return last_value;
        defer last_value.value.deinit();
        switch (last_value.value) {
            .runtime_error => |err| {
                std.debug.print("Runtime error: {s}\n", .{err.msg});
                if (err.extra) |extra| {
                    std.debug.print("{s}\n", .{extra});
                }
                if (err.pos) |pos| {
                    std.debug.print("{}\n", .{pos});
                    std.debug.print("line: {d}, column: {d}\n", .{ pos.line, pos.column });
                }
                return last_value; // if error in program return instantly
            },
            else => {},
        }
    }
    return last_value;
}

pub fn evalNode(self: Self, node: *const Node) rt.Result {
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
        .function_decl => self.evalFunctionDecl(node),
        .call => self.evalCall(node),
        .returnstmt => self.evalReturn(node),
        .array => self.evalArray(node),
        .index_access => self.evalIndex(node),
        .field_access => self.evalFieldAccess(node),
        .enum_decl => self.evalEnumDecl(node),
        .struct_decl => self.evalStructDecl(node),
        .field_decl => rt.Result.err("Not available", "Field declaration outside of struct", node.getPos()),
        .make => self.evalMake(node),
    };
}

fn staticEval(self: Self, node: *const Node) rt.Result {
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    defer arena.deinit();

    var symbols = SymbolTable.init(arena.allocator());
    symbols.parent = self.symbols;
    defer symbols.deinit();

    var interpreter = self.newScope(&symbols, arena.allocator());
    interpreter.static = true;

    return interpreter.evalNode(node);
}

fn evalBlock(self: Self, og_node: *const Node) rt.Result {
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    defer arena.deinit();

    const block_allocator = arena.allocator();
    var symbols = SymbolTable.init(block_allocator);
    defer symbols.deinit();

    symbols.parent = self.symbols;
    var interpreter = self.newScope(&symbols, self.allocator);

    var last_value: rt.Result = rt.Result.none();
    for (og_node.block.nodes) |node| {
        last_value = interpreter.evalNode(node);
        if (checkRuntimeErrorOrSignal(last_value, node)) |err| return err;
        last_value.value.deinit();
    }
    const value = switch (last_value) {
        .value => |val| rt.Result.val(val.clone()),
        else => last_value,
    };
    return value;
}

fn evalNumber(_: Self, node: tree.Number) rt.Result {
    return rt.Result.val(switch (node) {
        .integer => |i| rt.Value{ .integer = i.n },
        .float => |f| rt.Value{ .float = f.n },
    });
}

fn evalBinOp(self: Self, og_node: *const Node) rt.Result {
    const node = og_node.bin_op;
    const left = self.evalNode(node.left);

    if (checkRuntimeErrorOrSignal(left, node.left)) |err| return err;
    const val_left = left.value;
    // defer val_left.deinit();

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
    // defer val_right.deinit();

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

fn evalUnaryOp(self: Self, og_node: *const Node) rt.Result {
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

fn evalVarDecl(self: Self, og_node: *const Node) rt.Result {
    var ret_error: bool = false;

    const node = og_node.var_decl;
    const value: rt.Value = if (node.value) |v| blk: {
        const result = self.evalNode(v);
        if (checkRuntimeErrorOrSignal(result, v)) |err| return err;
        break :blk result.value;
    } else .none;

    defer if (ret_error) value.deinit();

    if (checkRuntimeError(value, node.value orelse og_node)) |err| {
        ret_error = true;
        return err;
    }

    var symval: SymbolTable.SymbolValue = undefined;
    const rs = rt.castToTypeWithErrorMessage(self.allocator, value.clone(), node.type, &symval);
    if (checkRuntimeErrorOrSignal(rs, og_node)) |err| {
        ret_error = true;
        return err;
    }
    defer if (ret_error) symval.deinit();

    const symbol = SymbolTable.Symbol{ .is_const = node.is_const.n, .value = symval, .type = node.type };

    self.symbols.add(node.identifier.lexeme, symbol) catch |err| {
        switch (err) {
            error.OutOfMemory => std.debug.panic("OUT OF MEMORY!!!", .{}),
            error.SymbolAlreadyExists => return rt.Result.err("Symbol already exists", "The symbol already exists", node.identifier.pos),
        }
    };

    return rt.Result.val(value);
}

fn evalVarAssign(self: Self, og_node: *const Node) rt.Result {
    const node = og_node.var_assign;
    const rtresult = self.evalNode(node.value);
    if (checkRuntimeErrorOrSignal(rtresult, node.value)) |err| return err;
    const orignal = rtresult.value;
    const value = orignal.clone();
    defer value.deinit();
    orignal.deinit();

    // it can either be a identifier or an index access
    switch (node.left.*) {
        .identifier => |ident| {
            const old_symbol = self.symbols.get(ident.lexeme) orelse {
                return rt.Result.err("Symbol not found", "The symbol was not found", ident.pos);
            };
            defer old_symbol.value.deinit();

            if (self.static) return rt.Result.val(rt.castToValue(old_symbol.value));

            const typeval = rt.getTypeValFromSymbolValue(old_symbol.value) catch {
                return rt.Result.err("Invalid Cast", "Can't convert the value into the type", og_node.getPos());
            };
            const symval = rt.castToSymbolValue(self.allocator, value, .{ .builtin = typeval }) catch {
                return rt.Result.err("Invalid Cast", "The value can't be converted to the type (could be due to the value is too big or small)", node.value.getPos());
            };

            self.symbols.set(ident.lexeme, symval) catch |err| switch (err) {
                error.SymbolDoesNotExist => return rt.Result.err("Symbol not found", "The symbol given was not found", ident.pos),
                error.SymbolIsImmutable => return rt.Result.err("Symbol is immutable", "The symbol is immutable (const)", og_node.getPos()),
                error.InvalidTypes => return rt.Result.err("Invalid Types", "Can't change a symbol's type", og_node.getPos()),
            };
            return rt.Result.val(rt.castToValue(symval).ref());
        },
        .index_access, .field_access => {
            const target = self.evalNode(node.left);
            if (checkRuntimeErrorOrSignal(target, node.left)) |err| return err;
            const target_val = target.value;
            // deinit twice because if not it DOESN'T WORK
            target_val.deinit();
            target_val.deinit();

            switch (target_val) {
                .ptr => |p| {
                    // we need to incrrement the ref count twice one for the pointer and one for the result
                    // because we deinit
                    p.* = value.ref();
                    return rt.Result.val(value.ref());
                },
                .symbol => |s| {
                    if (s.is_const) return rt.Result.err("Symbol is immutable", "The symbol is immutable (const)", og_node.getPos());
                    const rs = rt.castToTypeWithErrorMessage(self.allocator, value, s.type orelse blk: {
                        std.log.warn("Symbol has no type", .{});
                        break :blk Type.any();
                    }, &s.value);
                    if (checkRuntimeErrorOrSignal(rs, og_node)) |err| return err;
                    return rt.Result.val(value);
                },
                else => return rt.Result.err("Not Mutable", "The target is not mutable", node.left.getPos()),
            }
        },
        else => return rt.Result.err("Not Mutable", "The target is not mutable", node.left.getPos()),
    }
}

fn evalIfStmt(self: Self, og_node: *const Node) rt.Result {
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

fn evalForStmt(self: Self, og_node: *const Node) rt.Result {
    const node = og_node.forstmt;
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    defer arena.deinit();

    var symbols = SymbolTable.init(arena.allocator());
    symbols.parent = self.symbols;
    defer symbols.deinit();

    const outer_scope = self.newScope(&symbols, self.allocator);
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

        const inner_scope = self.newScope(&inner_symbols, self.allocator);

        const body = inner_scope.evalNode(node.body);
        if (checkRuntimeErrorOrSignal(body, node.body)) |sigOrErr| switch (sigOrErr) {
            .signal => |signal| switch (signal) {
                .@"break" => |v| return rt.Result.val(v),
                .@"continue" => {},
                else => return sigOrErr,
            },
            else => return sigOrErr,
        };
        defer body.value.deinit();

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

fn evalWhileStmt(self: Self, og_node: *const Node) rt.Result {
    const node = og_node.whilestmt;
    while (true) {
        const condition = self.evalNode(node.condition);
        if (checkRuntimeErrorOrSignal(condition, node.condition)) |err| return err; // if we get a signal like continue or break ignore it in the condition
        const val_cond = condition.value;

        const boolean_value = val_cond.convertToBool();
        if (checkRuntimeError(boolean_value, node.condition)) |err| return err;

        if (!boolean_value.boolean) break; // break out of loop

        const body = self.evalNode(node.body);
        defer body.value.deinit();
        if (checkRuntimeErrorOrSignal(body, node.body)) |sigOrErr| switch (sigOrErr) {
            .signal => |signal| switch (signal) {
                .@"break" => |v| return rt.Result.val(v),
                .@"continue" => {},
                else => return sigOrErr,
            },
            else => return sigOrErr,
        };
    }

    return rt.Result.none();
}

fn evalIdentifier(self: Self, og_node: *const Node) rt.Result {
    const token = og_node.identifier;
    const symbol = self.symbols.get(token.lexeme) orelse {
        return rt.Result.err("Symbol not found", "The symbol was not found", token.pos);
    };
    return rt.Result.val(rt.castToValue(symbol.value));
}

fn evalBreak(self: Self, og_node: *const Node) rt.Result {
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

fn evalString(self: Self, og_node: *const Node) rt.Result {
    const node = og_node.string;
    defer if (!self.static) node.deinit();
    return rt.Result.val(rt.Value.str(node, self.heap_str_only, self.allocator) catch @panic("OUT OF MEMORY"));
}

fn evalCharacter(self: Self, og_node: *const Node) rt.Result {
    _ = self;
    const node = og_node.char;
    return rt.Result.val(.{ .char = node.n });
}

fn evalFunctionDecl(self: Self, og_node: *const Node) rt.Result {
    const node = og_node.function_decl;

    const function = ty.BaseFunction{
        .name = node.identifier.lexeme,
        .params = node.params,
        .body = node.body,
        .return_type = node.ret_type,
        .parent_scope = self.symbols,
        // debug pos
        .name_pos = node.identifier.pos,
        .return_type_pos = node.ret_type.pos,

        .outer_type = null,
        .func_type = if (node.is_make) .make else .static,
    };

    if (self.static_checking) {
        if (function.staticCheck(self)) |sigOrErr| switch (sigOrErr) {
            .signal => return rt.Result.err("Static Error", "unhandled signal", node.identifier.pos),
            else => return sigOrErr,
        };
    }

    // check if return type matches function body
    self.symbols.add(node.identifier.lexeme, SymbolTable.Symbol{
        .is_const = true, // functions are always const
        .value = .{ .func = .{ .base = function } },
    }) catch |err| switch (err) {
        error.OutOfMemory => std.debug.panic("OUT OF MEMORY!!!", .{}),
        error.SymbolAlreadyExists => return rt.Result.err("Symbol already exists", "The symbol already exists", node.identifier.pos),
    };
    return rt.Result.none(); // decl function does not return a value
}

fn getArgs(self: Self, buf: []rt.Value, args: []const *const Node) union(enum) { res: rt.Result, args: []const rt.Value } {
    var index: usize = 0;
    for (args) |arg| {
        const arg_value = self.evalNode(arg);
        if (checkRuntimeErrorOrSignal(arg_value, arg)) |err| return .{ .res = err };
        buf[index] = arg_value.value;
        index += 1;
    }
    return .{ .args = buf[0..index] };
}

fn handleFunctionResult(self: Self, func: ty.Function, result: rt.Result, og_node: *const Node) rt.Result {
    const pos: ?Pos = switch (func) {
        .base => |f| f.name_pos,
        .bultin => og_node.getPos(),
    };
    if (checkRuntimeErrorOrSignal(result, og_node)) |sigOrErr| switch (sigOrErr) {
        .signal => |signal| switch (signal) {
            .@"return" => |v| switch (func) {
                .base => |basef| {
                    defer v.deinit();

                    var typed_value: SymbolTable.SymbolValue = undefined;
                    const rs = rt.castToTypeWithErrorMessage(self.allocator, v.clone(), basef.return_type, &typed_value);
                    if (checkRuntimeErrorOrSignal(rs, og_node)) |err| return err;
                    const value = rt.castToValueNoRef(typed_value);
                    return rt.Result.val(value);
                },
                .bultin => {
                    if (checkRuntimeErrorOrSignal(result, og_node)) |err| return err;
                    return result;
                },
            },
            else => return rt.Result.err("Unexpected signal", "The function resulted in an unexpected signal", pos),
        },
        else => return sigOrErr,
    };
    switch (func) {
        .base => |basef| {
            if (!basef.return_type.value.eqlBuiltin(.void)) {
                const msg = std.fmt.allocPrint(self.allocator, "Function return an expected type, Expected {s} got void", .{basef.return_type.getName()}) catch unreachable;
                return rt.Result.errHeap(self.allocator, "Invalid return type", msg, func.base.return_type_pos);
            }
            return rt.Result.none();
        },
        .bultin => {
            if (checkRuntimeErrorOrSignal(result, og_node)) |err| return err;
            return result;
        },
    }
}

fn callValue(self: Self, og_node: *const Node, callee_value: ?rt.Value, callee_pos: ?Pos, value: rt.Value, args: []const *const Node) rt.Result {
    switch (value) {
        .func => |func| {
            if (func == .base and func.base.func_type == .make) {
                return rt.Result.err("Cannot call make function", "Cannot call make function", callee_pos);
            }

            var args_buf: [256]rt.Value = undefined; // 256 is being geneous just in case the user tries to push the limit
            const args_result = self.getArgs(&args_buf, args);
            if (args_result == .res) {
                return args_result.res;
            }
            const actual_args = args_result.args;

            if (callee_value) |callee| {
                const ret = func.callWithType(callee.ref(), actual_args, self);
                return self.handleFunctionResult(func, ret, og_node);
            }
            const ret = func.call(actual_args, self);
            return self.handleFunctionResult(func, ret, og_node);
        },
        else => return rt.Result.err("Not a function", "The symbol is not a function", callee_pos),
    }
}

fn evalCall(self: Self, og_node: *const Node) rt.Result {
    const node = og_node.call;
    switch (node.callee.*) {
        .identifier => |ident| {
            const callee = self.symbols.get(ident.lexeme) orelse {
                return rt.Result.err("Symbol not found", "The symbol was not found", ident.pos);
            };
            return self.callValue(og_node, null, ident.pos, rt.castToValueNoRef(callee.value), node.args);
        },
        .field_access => |field_access| {
            const base = self.evalNode(field_access.value);
            if (checkRuntimeErrorOrSignal(base, og_node)) |err| return err;
            const bvalue = base.value;

            const field = bvalue.field(field_access.field.lexeme);
            if (checkRuntimeError(field, og_node)) |err| return err;

            return self.callValue(og_node, bvalue, field_access.field.pos, field.depointerizeToValue(), node.args);
        },
        else => return rt.Result.err("Not a callable", "Can't call a non-callable", node.callee.getPos()),
    }
}

fn evalReturn(self: Self, og_node: *const Node) rt.Result {
    const node = og_node.returnstmt;
    if (node.value) |val_node| {
        const result = self.evalNode(val_node);
        if (checkRuntimeErrorOrSignal(result, og_node)) |err| return err;
        const value = result.value;

        return rt.Result.sig(.{ .@"return" = value });
    }

    return rt.Result.sig(.{ .@"return" = .none });
}

fn evalArray(self: Self, og_node: *const Node) rt.Result {
    const node = og_node.array;
    defer if (!self.static) node.deinit();

    const list = ty.List.initMutable(self.allocator); // lists are naturally immutable (meaning they cannot change the items within)
    defer list.deinit();
    list.resize(node.items.len) catch unreachable;

    for (node.items) |item| {
        const result = self.evalNode(item);
        if (checkRuntimeErrorOrSignal(result, og_node)) |err| return err;
        defer result.value.deinit();
        list.append(result.value.clone()) catch unreachable;
    }

    return rt.Result.val(.{ .list = list.clone().recursiveMutablity(true) });
}

fn evalIndex(self: Self, og_node: *const Node) rt.Result {
    const node = og_node.index_access;
    const value = self.evalNode(node.value);
    if (checkRuntimeErrorOrSignal(value, node.value)) |err| return err;
    const index = self.evalNode(node.index);
    if (checkRuntimeErrorOrSignal(index, node.index)) |err| return err;
    const val_value = value.value;
    const val_index = index.value;

    const value_at_index = val_value.index(val_index);
    if (checkRuntimeError(value_at_index, og_node)) |err| return err;
    return rt.Result.val(value_at_index);
}

fn evalFieldAccess(self: Self, og_node: *const Node) rt.Result {
    const node = og_node.field_access;
    const result = self.evalNode(node.value);
    if (checkRuntimeErrorOrSignal(result, node.value)) |err| return err;
    const value = result.value;
    const field = node.field;

    const value_at_field = value.field(field.lexeme);
    if (checkRuntimeError(value_at_field, og_node)) |err| return err;
    return rt.Result.val(value_at_field);
}

fn evalEnumDecl(self: Self, og_node: *const Node) rt.Result {
    const node = og_node.enum_decl;
    defer if (!self.static) node.deinit();

    var fields = std.ArrayList(ty.Enum.Field).init(self.allocator);
    defer fields.deinit();

    var last_value: rt.Value = .{ .integer = -1 };
    for (node.fields, 0..) |field, i| {
        if (field.value) |val_node| {
            const result = self.evalNode(val_node);
            if (checkRuntimeErrorOrSignal(result, og_node)) |err| return err;
            last_value = result.value;
        } else {
            last_value = rt.Value.add(self.allocator, last_value, .{ .integer = 1 });
            if (checkRuntimeError(last_value, og_node)) |err| return err;
        }
        // right now it hard coded to i32
        const field_value = rt.castToType(self.allocator, last_value, node.tag_type) catch {
            return rt.Result.err("Invalid enum value", "The value can't be converted to tagged type", field.name.pos);
        };

        // make sure the field is unique
        for (fields.items) |f| {
            if (std.mem.eql(u8, f.name, field.name.lexeme)) {
                return rt.Result.err("Duplicate field", "The field is already defined", field.name.pos);
            }

            if (f.value.equal(field_value)) {
                return rt.Result.errPrint(self.allocator, "Duplicate value", "The value({}) is already defined", .{f.value}, field.name.pos);
            }
        }

        fields.append(.{
            .name = field.name.lexeme,
            .value = field_value,
            .backing_value = @intCast(i),
        }) catch return rt.Result.err("Out of memory", "The interpreter ran out of memory", og_node.getPos());
    }

    const enum_decl = ty.Enum.init(self.allocator, node.identifier.lexeme, fields.toOwnedSlice() catch unreachable, node.tag_type);
    const ptr = self.symbols.addGetPtr(node.identifier.lexeme, .{
        .is_const = true, // declared enums are always const
        .value = .{ .@"enum" = enum_decl },
    }) catch unreachable;

    const global_uuid = Type.setType(self.allocator, node.identifier.lexeme, ptr);
    ptr.value.@"enum".global_uuid = global_uuid;

    return rt.Result.none();
}

fn evalStructDecl(self: Self, og_node: *const Node) rt.Result {
    var is_error = false;

    const node = og_node.struct_decl;
    defer if (!self.static) node.deinit();

    const inner_table = self.allocator.create(SymbolTable) catch unreachable;
    inner_table.* = SymbolTable.init(self.allocator);
    defer if (is_error) inner_table.deinit(); // errdefer

    inner_table.parent = self.symbols;

    var fields = std.ArrayList(ty.Struct.Field).init(self.allocator);
    defer fields.deinit();

    var scope = self.newScope(inner_table, self.allocator);
    scope.static_checking = false; // disable static checking

    for (node.inner) |inner_node| {
        const result = switch (inner_node.*) {
            .field_decl => |field| result: {
                const value: ?rt.Value = if (field.value) |val| get_value: {
                    const result = scope.evalNode(val);
                    if (checkRuntimeErrorOrSignal(result, val)) |err| {
                        is_error = true;
                        return err;
                    }
                    break :get_value result.value;
                } else null;

                fields.append(.{
                    .type = field.type,
                    .name = field.identifier.lexeme,
                    .default = value,
                }) catch return rt.Result.err("Out of memory", "The interpreter ran out of memory", og_node.getPos());

                break :result rt.Result.none();
            },
            else => scope.evalNode(inner_node),
        };
        if (checkRuntimeErrorOrSignal(result, inner_node)) |err| {
            is_error = true;
            return err;
        }
        result.deinit(); // we do not wanna keep result
    }

    if (is_error)
        return rt.Result.err("Unexpected error", "The interpreter ran into an unexpected error", og_node.getPos());

    const struct_decl = ty.Struct.init(node.identifier.lexeme, inner_table, fields.toOwnedSlice() catch unreachable);
    const ptr = self.symbols.addGetPtr(node.identifier.lexeme, .{
        .is_const = true, // declared structs are always const
        .value = .{ .@"struct" = struct_decl },
    }) catch unreachable;

    const global_uuid = Type.setType(self.allocator, node.identifier.lexeme, ptr);
    ptr.value.@"struct".global_uuid = global_uuid;

    var it = struct_decl.inner.table.iterator();
    while (it.next()) |entry| {
        switch (entry.value_ptr.value) {
            .func => |*f| {
                const base = &f.base;
                base.outer_type = Type.getTypeFromUUID(global_uuid);
                if (base.staticCheck(scope)) |sigOrErr| switch (sigOrErr) {
                    .signal => return rt.Result.err("Static Error", "unhandled signal", node.identifier.pos),
                    else => return sigOrErr,
                };
            },
            else => {},
        }
    }

    return rt.Result.none();
}

fn makeNotFound(instance: *ty.Struct.Instance, strict: bool, arg_len: usize, type_pos: ?Pos, func_pos: ?Pos) rt.Result {
    if (strict) return rt.Result.err("Undefined Make", "The Make Function is not defined", type_pos);
    if (arg_len != 0) return rt.Result.err("Unexpected arguments", "The Make Function doesn't take arguments", func_pos);
    return rt.Result.val(.{ .struct_instance = instance });
}

fn callMakeFn(self: Self, name: []const u8, args: []const *const Node, strict: bool, s: ty.Struct, type_pos: ?Pos) rt.Result {
    const instance = s.makeNone(self.allocator) catch |err| {
        return rt.Result.errPrint(self.allocator, "Make Error", "Failed to make struct due to {s}", .{@errorName(err)}, null);
    };

    const sym = s.inner.get(name) orelse return makeNotFound(instance, strict, args.len, type_pos, null);

    // guard checks to make sure it is a make function (if not ignore it since we didn't pass in a name)
    if (sym.value != .func) return makeNotFound(instance, strict, args.len, type_pos, null);
    if (sym.value.func != .base) return makeNotFound(instance, strict, args.len, type_pos, null);
    if (sym.value.func.base.func_type != .make) return makeNotFound(instance, strict, args.len, type_pos, sym.value.func.base.name_pos);

    const mkfn = sym.value.func.base;
    if (args.len != mkfn.params.len) {
        return rt.Result.errPrint(
            self.allocator,
            "Unexpected arguments",
            "The Make Functions takes {d} arguments while {d} were given",
            .{ mkfn.params.len, args.len },
            null,
        );
    }

    if (!mkfn.return_type.value.eqlBuiltin(.void)) return rt.Result.err("Invalid return type", "The Make Function must return void", mkfn.return_type_pos);

    var arg_bug: [256]rt.Value = undefined;
    const args_result = self.getArgs(&arg_bug, args);
    if (args_result == .res) {
        return args_result.res;
    }
    const actual_args = args_result.args;

    std.log.debug("Calling Make Function: {s}", .{name});
    const result = mkfn.callWithType(rt.Value{ .struct_instance = instance.ref() }, actual_args, self);
    switch (result) {
        .value => |val| if (val == .runtime_error) return result,
        .signal => |sig| switch (sig) {
            .@"return" => |val| {
                if (val != .none) return rt.Result.err("Invalid return value", "The Make Function must return void", mkfn.name_pos);
            },
            else => return rt.Result.err("Signal", "The Make Function return an invalid signal", mkfn.name_pos),
        },
    }

    return rt.Result{ .value = .{ .struct_instance = instance } };
}

fn evalMake(self: Self, og_node: *const Node) rt.Result {
    const node = og_node.make;

    switch (node.type.value) {
        .builtin => {
            if (node.args.len != 0) return rt.Result.err("Unexpected arguments", "The Make Function doesn't take arguments", og_node.getPos());
            const value = rt.castToType(self.allocator, .none, node.type) catch unreachable;

            return rt.Result{ .value = rt.castToValueNoRef(value) };
        },
        .defined => |deft| {
            switch (deft.ty.value) {
                .@"enum" => |e| {
                    if (node.args.len != 0) return rt.Result.err("Unexpected arguments", "The Make Function doesn't take arguments", og_node.getPos());

                    const field_name = e.fields[0].name;
                    const instance = e.field(field_name); // get the first field instance

                    return rt.Result{ .value = instance };
                },
                .@"struct" => |s| {
                    const name = if (node.name) |n| n.lexeme else "_";
                    const strict = node.name != null;

                    const result = self.callMakeFn(name, node.args, strict, s, node.type.pos);
                    if (checkRuntimeErrorOrSignal(result, og_node)) |err| return err;
                    return result;
                },
                else => return rt.Result.err("Invalid type", "The type is not a valid type", og_node.getPos()),
            }
        },
    }
}
