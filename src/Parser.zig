const std = @import("std");
const Token = @import("Token.zig");
const Type = @import("Type.zig");
const tree = @import("tree.zig");
const DebugPos = @import("DebugPos.zig");

const Self = @This();

tokens: []const Token,
index: usize = 0,
prev: ?Token = null,
node_allocator: std.mem.Allocator, // quicky because use for nodes
allocator: std.mem.Allocator, // use for invidual heap needed values

const ParseError = error{
    UnexpectedToken,
    ExpectedStatement,
    ExpectedToken,
    InvalidToken,
    ExpectedSemicolon,
    ExpectedEqual,
    ExpectedIdentifier,
    ExpectedType,
    ExpectedElse,
    OutOfMemory,
    MissingOperator,
    MissingParen,
    MissingBracket,
    MissingCurlyBracket,
    MissingAngleBracket,
    NumberOverflow,
    InvalidNumber,
    InvalidAssignmentTarget,
    InvalidCharacter,
    MissingEndOfString,
    NotAGeneric,
    ExpectedGenericType,
};

pub fn init(tokens: []const Token, node_allocator: std.mem.Allocator, allocator: std.mem.Allocator) Self {
    return Self{ .tokens = tokens, .index = 0, .node_allocator = node_allocator, .allocator = allocator };
}

pub fn deinit(self: Self, nodes: []const *const tree.Node) void {
    for (nodes) |node| {
        node.deinit();
        self.node_allocator.destroy(node);
    }
}

fn current(self: Self) ?Token {
    return if (self.index < self.tokens.len) self.tokens[self.index] else null;
}

pub fn previous(self: Self) ?Token {
    return if (self.index > 0) self.tokens[self.index - 1] else null;
}

fn advance(self: *Self) ?Token {
    const token = self.current();
    self.prev = token;
    self.index = std.math.clamp(self.index + 1, 0, self.tokens.len);
    return token;
}

fn match(self: Self, kind: Token.Kind) bool {
    return self.current() != null and self.current().?.kind == kind;
}

fn matchP(self: Self, kind: Token.Kind, n: usize) bool {
    return self.peekN(n) != null and self.peekN(n).?.kind == kind;
}

fn consume(self: *Self, kind: Token.Kind) ?Token {
    if (self.match(kind)) {
        return self.advance();
    }
    return null;
}

fn peek(self: Self) ?Token {
    return self.peekN(1);
}

fn peekN(self: Self, n: usize) ?Token {
    return if (self.index + n < self.tokens.len) self.tokens[self.index + n] else null;
}

fn allocNode(self: *Self, node: tree.Node) !*const tree.Node {
    const ptr = try self.node_allocator.create(tree.Node);
    ptr.* = node;
    return ptr;
}

pub fn parse(self: *Self) ParseError![]const *const tree.Node {
    var nodes: std.ArrayList(*const tree.Node) = std.ArrayList(*const tree.Node).init(self.node_allocator);
    errdefer {
        self.deinit(nodes.items);
        nodes.deinit();
    }
    while (try self.parseInstruction()) |node| {
        try nodes.append(node);
    }
    return nodes.toOwnedSlice();
}

pub fn parseInstruction(self: *Self) ParseError!?*const tree.Node {
    if (self.current() == null) return null;
    return self.parseBlockOr(&parseStatement); // if no block parse statement
}

const ParseFn = *const fn (*Self) ParseError!*const tree.Node;
fn parseBinaryOperand(self: *Self, operators: []const Token.Kind, next: ParseFn) ParseError!*const tree.Node {
    const Context = struct {
        fn check(s: *Self, ops: []const Token.Kind) bool {
            for (ops) |op| {
                if (s.consume(op)) |_| {
                    return true;
                }
            }
            return false;
        }
    };

    var left = try next(self);
    while (Context.check(self, operators)) {
        const operator = self.previous() orelse return error.MissingOperator;
        const right = try next(self);
        left = try self.allocNode(tree.Node{
            .bin_op = .{ .left = left, .op = operator, .right = right },
        });
    }

    return left;
}

fn parseBlockOr(self: *Self, next: ParseFn) ParseError!*const tree.Node {
    if (self.match(.left_curly_bracket)) {
        return self.parseBlock();
    }

    return next(self);
}

fn parseBlock(self: *Self) ParseError!*const tree.Node {
    if (self.consume(.left_curly_bracket)) |start| {
        var nodes: std.ArrayList(*const tree.Node) = std.ArrayList(*const tree.Node).init(self.node_allocator);
        errdefer {
            for (nodes.items) |node| self.node_allocator.destroy(node);
            nodes.deinit();
        }

        while (self.current() != null and !self.match(.right_curly_bracket)) {
            if (try self.parseInstruction()) |node| { // or should we use try self.parseStatement()?
                try nodes.append(node);
            }
        }

        const end = self.consume(.right_curly_bracket) orelse return self.badToken(error.MissingCurlyBracket);

        return self.allocNode(tree.Node{
            .block = .{
                .start = start,
                .nodes = try nodes.toOwnedSlice(),
                .end = end,
            },
        });
    }

    return error.MissingCurlyBracket;
}

fn parseStatement(self: *Self) ParseError!*const tree.Node {
    if (self.match(.if_kw)) {
        const node = try self.parseIf(false);
        if (node.ifstmt.isBlockless()) {
            if (self.consume(.semicolon)) |_| {
                return node;
            }
            return error.ExpectedSemicolon;
        } else {
            return node;
        }
    }
    if (self.match(.for_kw)) {
        const node = try self.parseFor(false);
        if (node.forstmt.isBlockless()) {
            if (self.consume(.semicolon)) |_| {
                return node;
            }
            return error.ExpectedSemicolon;
        } else {
            return node;
        }
    }
    if (self.match(.while_kw)) {
        const node = try self.parseWhile();
        if (node.whilestmt.isBlockless()) {
            if (self.consume(.semicolon)) |_| {
                return node;
            }
            return error.ExpectedSemicolon;
        } else {
            return node;
        }
    }
    if (self.match(.func_kw)) {
        return try self.parseFunction();
    }
    if (self.consume(.break_kw)) |st| {
        if (self.consume(.semicolon)) |_| {
            return self.allocNode(tree.Node{ .breakstmt = .{ .start = st, .value = null } });
        }

        const value = try self.parseExprWithSemicolon(); // so we can do break i; and we have less code in this function
        return self.allocNode(tree.Node{ .breakstmt = .{ .start = st, .value = value } });
    }
    if (self.consume(.continue_kw)) |st| {
        if (self.consume(.semicolon)) |_| {
            return self.allocNode(tree.Node{ .continuestmt = st });
        }
        return error.ExpectedSemicolon;
    }
    if (self.consume(.return_kw)) |st| {
        if (self.consume(.semicolon)) |_| {
            return self.allocNode(tree.Node{ .returnstmt = .{ .start = st, .value = null } });
        }

        const value = try self.parseExprWithSemicolon();
        return self.allocNode(tree.Node{ .returnstmt = .{ .start = st, .value = value } });
    }
    return self.parseExprWithSemicolon();
}

fn parseExprWithSemicolon(self: *Self) ParseError!*const tree.Node {
    const expr = try self.parseExpression();
    if (self.consume(.semicolon)) |_| {
        return expr;
    }
    return error.ExpectedSemicolon;
}

inline fn badToken(self: *Self, err: ParseError) ParseError {
    _ = self.advance();
    return err;
}

fn parseExpression(self: *Self) ParseError!*const tree.Node {
    if (self.match(.var_kw) or self.match(.const_kw)) {
        return self.parseVariableDecl();
    }
    // use match instead of consume because the parseIf checks for the if keyword
    if (self.match(.left_curly_bracket)) return self.parseBlockOr(&parseExpression);
    if (self.match(.if_kw)) return self.parseIf(true);
    if (self.match(.for_kw)) return self.parseFor(true);
    if (self.match(.while_kw)) return self.parseWhile();

    return self.parseAssign();
}

fn parseType(self: *Self) ParseError!Type {
    const tyval = self.consume(.type) orelse return self.badToken(error.ExpectedType);

    const generic_type: ?*const Type = if (self.consume(.lt)) |_| blk: {
        const _type = try self.parseType();
        _ = self.consume(.gt) orelse return self.badToken(error.MissingAngleBracket);

        const ptr = try self.node_allocator.create(Type);
        ptr.* = _type;
        break :blk ptr;
    } else null;

    var ty = Type{ .value = tyval.value.type, .generic_type = generic_type };
    ty.pos = tyval.pos;

    try ty.checkGenericError(); // returns generic error if the type is not generic or needs a generic type

    return ty;
}

fn parseVariableDecl(self: *Self) ParseError!*const tree.Node {
    const tok = self.advance().?; // should never be null
    const is_const = tok.kind == .const_kw;
    const t = try self.parseType();

    const identifier = self.consume(.identifier) orelse return self.badToken(error.ExpectedIdentifier);
    const value: ?*const tree.Node = if (self.consume(.equal)) |_| try self.parseExpression() else null;

    return self.allocNode(.{
        .var_decl = .{
            .is_const = .{ .n = is_const, .orginal = tok },
            .type = t,
            .identifier = identifier,
            .value = value,
        },
    });
}

fn parseAssign(self: *Self) ParseError!*const tree.Node {
    const lhs = try self.parseVariableAssignOp();
    if (self.consume(.equal)) |_| {
        switch (lhs.*) {
            .identifier, .index_access => {},
            else => return error.InvalidAssignmentTarget,
        }
        const rhs = try self.parseExpression();
        return self.allocNode(tree.Node{ .var_assign = .{ .left = lhs, .value = rhs } });
    }
    return lhs;
}

fn parseVariableAssignOp(self: *Self) ParseError!*const tree.Node {
    const lhs = try self.parseLogicalOr();
    if (self.peekNextIsCompoundAssignOp()) {
        return self.desurgarCompoundAssign(lhs);
    }

    return lhs;
}

fn peekNextIsCompoundAssignOp(self: *Self) bool {
    const p = self.current() orelse return false;

    return switch (p.kind) {
        .plus_equal, .minus_equal, .star_equal, .slash_equal, .plus_plus, .minus_minus => true,
        else => false,
    };
}

fn desurgarCompoundAssign(self: *Self, lhs: *const tree.Node) ParseError!*const tree.Node {
    switch (lhs.*) {
        .identifier, .index_access => {},
        else => return error.InvalidAssignmentTarget,
    }
    const op = self.advance().?;

    const new_op: Token.Kind = switch (op.kind) {
        .plus_equal => .plus,
        .minus_equal => .minus,
        .star_equal => .star,
        .slash_equal => .slash,
        .plus_plus => .plus,
        .minus_minus => .plus,
        else => unreachable,
    };
    const new_tok = Token{
        .kind = new_op,
        .lexeme = op.lexeme,
        .pos = op.pos,
    };

    if (op.kind == .plus_plus or op.kind == .minus_minus) {
        const bin_op = try self.allocNode(tree.Node{
            .bin_op = .{
                .left = lhs,
                .op = new_tok,
                .right = try self.allocNode(tree.Node{ .number = .{ .integer = .{ .n = 1, .orginal = op } } }),
            },
        });
        return self.allocNode(tree.Node{ .var_assign = .{ .left = lhs, .value = bin_op } });
    }

    const rhs = try self.parseExpression();

    const bin_op = try self.allocNode(tree.Node{
        .bin_op = .{
            .left = lhs,
            .op = new_tok,
            .right = rhs,
        },
    });
    return self.allocNode(tree.Node{ .var_assign = .{ .left = lhs, .value = bin_op } });
}

fn parseIf(self: *Self, expect_value: bool) ParseError!*const tree.Node {
    const start = self.consume(.if_kw) orelse return self.badToken(error.UnexpectedToken); // should not happen so we don't make the error clear
    _ = self.consume(.left_paren) orelse return self.badToken(error.MissingParen);
    // parse the condition (expression)
    const condition = try self.parseExpression();
    _ = self.consume(.right_paren) orelse return self.badToken(error.MissingParen);

    // const then = try if (self.match(.left_curly_bracket)) self.parseBlock() else self.parseExpression();
    const then = try self.parseBlockOr(&parseExpression); // because we don't want to parse a statement (aka expr with semicolon)
    // if this was parse statement it will look like this
    // const u8 x = if (true) 10;; because were already parsing a statement
    if (expect_value) { // gurantee we have an else
        if (!self.match(.if_kw) and !self.match(.else_kw)) {
            return error.ExpectedElse;
        }
    }

    if (self.consume(.else_kw)) |_| {
        if (self.match(.if_kw)) {
            const else_node = try self.parseIf(expect_value);
            return self.allocNode(tree.Node{
                .ifstmt = .{
                    .start = start,
                    .condition = condition,
                    .then = then,
                    .else_node = else_node,
                },
            });
        }
        const else_node = try self.parseBlockOr(&parseExpression);
        return self.allocNode(tree.Node{
            .ifstmt = .{
                .start = start,
                .condition = condition,
                .then = then,
                .else_node = else_node,
            },
        });
    }

    return self.allocNode(tree.Node{
        .ifstmt = .{
            .start = start,
            .condition = condition,
            .then = then,
            .else_node = null,
        },
    });
}

fn parseFor(self: *Self, expect_value: bool) ParseError!*const tree.Node {
    const start = self.consume(.for_kw) orelse return self.badToken(error.UnexpectedToken);
    _ = self.consume(.left_paren) orelse return self.badToken(error.MissingParen);

    // parse the start statement (aka. var u64 i = 0;)
    // parse the condition expression (aka, i < 10;)
    // parse the every iteration statement (aka. i++;)

    const start_statement = try self.parseStatement();
    const condition = try self.parseExprWithSemicolon();
    const every_iteration = try self.parseStatement();

    _ = self.consume(.right_paren) orelse return self.badToken(error.MissingParen);

    const body = try self.parseBlockOr(&parseExpression);
    if (expect_value and !self.match(.else_kw)) return error.ExpectedElse;

    const else_node = if (self.consume(.else_kw)) |_| try self.parseBlockOr(&parseExpression) else null;

    return self.allocNode(tree.Node{
        .forstmt = .{
            .start = start,
            .start_statement = start_statement,
            .condition = condition,
            .every_iteration = every_iteration,
            .body = body,
            .else_node = else_node,
        },
    });
}

fn parseWhile(self: *Self) ParseError!*const tree.Node {
    const start = self.consume(.while_kw) orelse return self.badToken(error.UnexpectedToken);

    _ = self.consume(.left_paren) orelse return self.badToken(error.MissingParen);
    const condition = try self.parseExpression();
    _ = self.consume(.right_paren) orelse return self.badToken(error.MissingParen);

    const body = try self.parseBlockOr(&parseStatement);

    return self.allocNode(tree.Node{
        .whilestmt = .{
            .start = start,
            .condition = condition,
            .body = body,
        },
    });
}

fn parseLogicalOr(self: *Self) ParseError!*const tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{.pipe_pipe}, &parseLogicalAnd);
}

fn parseLogicalAnd(self: *Self) ParseError!*const tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{.ampersand_ampersand}, &parseBitor);
}

fn parseBitor(self: *Self) ParseError!*const tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{.pipe}, &parseXor);
}

fn parseXor(self: *Self) ParseError!*const tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{.caret}, &parseBitand);
}

fn parseBitand(self: *Self) ParseError!*const tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{.ampersand}, &parseEquality);
}

fn parseEquality(self: *Self) ParseError!*const tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{ .equal_equal, .bang_equal }, &parseRelational);
}

fn parseRelational(self: *Self) ParseError!*const tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{ .lt, .gt, .lt_equal, .gt_equal }, &parseShift);
}

fn parseShift(self: *Self) ParseError!*const tree.Node {
    const Context = struct {
        fn check(s: *Self) bool {
            if (s.match(.lt) and s.matchP(.lt, 1)) return true;
            if (s.match(.gt) and s.matchP(.gt, 1)) return true;
            return false;
        }
    };
    var left = try self.parseTerm();
    while (Context.check(self)) {
        const tok = self.advance().?;
        const tok2 = self.consume(tok.kind) orelse return self.badToken(error.ExpectedToken);
        std.debug.assert(tok2.kind == tok.kind);
        var shift_token = if (tok.kind == .lt)
            Token.init(.lt_lt, "<<")
        else
            Token.init(.gt_gt, ">>");

        const pos: ?DebugPos = blk: {
            if (tok.pos) |p1| {
                if (tok2.pos) |p2| {
                    break :blk p1.combine(p2);
                }
                break :blk p1;
            }
            if (tok2.pos) |p2| {
                break :blk p2;
            }
            break :blk null;
        };
        shift_token.pos = pos;

        const right = try self.parseTerm();
        left = try self.allocNode(tree.Node{
            .bin_op = .{ .left = left, .op = shift_token, .right = right },
        });
    }
    return left;
}

fn parseTerm(self: *Self) ParseError!*const tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{ .plus, .minus }, &parseFactor);
}

fn parseFactor(self: *Self) ParseError!*const tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{ .star, .slash, .percent }, &parseUnary);
}

fn parseUnary(self: *Self) ParseError!*const tree.Node {
    if (self.consume(.minus)) |tok| {
        const number = try self.parseUnary();
        return self.allocNode(tree.Node{ .unary_op = .{ .op = tok, .right = number } });
    }
    if (self.consume(.plus)) |_| {
        const number = try self.parseUnary();
        return number; // ignore +
    }
    if (self.consume(.bang)) |tok| {
        const right = try self.parseUnary();
        return self.allocNode(tree.Node{ .unary_op = .{ .op = tok, .right = right } });
    }

    return self.parsePrimary();
}

fn parsePrimary(self: *Self) ParseError!*const tree.Node {
    const base = try self.parseBasePrimary();
    return self.parsePosfix(base);
}

fn parsePosfix(self: *Self, left: *const tree.Node) ParseError!*const tree.Node {
    if (self.consume(.left_paren)) |_| {
        if (self.consume(.right_paren)) |right| {
            const new_node = try self.allocNode(tree.Node{
                .call = .{ .callee = left, .args = &.{}, .end = right },
            });
            return self.parsePosfix(new_node);
        }

        const args = try self.parseArguments();
        const right = self.consume(.right_paren) orelse return self.badToken(error.MissingParen);
        const new_node = try self.allocNode(tree.Node{
            .call = .{ .callee = left, .args = args, .end = right },
        });
        return self.parsePosfix(new_node);
    }

    if (self.consume(.left_bracket)) |_| {
        const index = try self.parseExpression();
        _ = self.consume(.right_bracket) orelse return self.badToken(error.MissingBracket);

        const new_node = try self.allocNode(tree.Node{
            .index_access = .{ .value = left, .index = index },
        });

        return self.parsePosfix(new_node);
    }

    if (self.consume(.dot)) |_| {
        const identifier = self.consume(.identifier) orelse return self.badToken(error.ExpectedIdentifier);
        const new_node = try self.allocNode(tree.Node{
            .field_access = .{ .value = left, .field = identifier },
        });

        return self.parsePosfix(new_node);
    }

    return left;
}

// add suffixes so when we parse primary we can then parse a suffix such as [0] or ()
fn parseBasePrimary(self: *Self) ParseError!*const tree.Node {
    if (self.consume(.left_paren)) |_| {
        const expr = try self.parseExpression();
        if (self.consume(.right_paren)) |_| {
            return expr;
        }
        return error.MissingParen;
    }
    if (self.consume(.number)) |tok| {
        std.debug.assert(tok.lexeme.len > 0);
        const dot_count = std.mem.count(u8, tok.lexeme, ".");
        std.debug.assert(dot_count <= 1); // the lexer should have caught this already
        if (dot_count == 0) {
            const integer = std.fmt.parseInt(i65, tok.lexeme, 0) catch |err| switch (err) {
                error.Overflow => return error.NumberOverflow,
                error.InvalidCharacter => return error.InvalidNumber,
            };
            return self.allocNode(tree.Node{ .number = .{ .integer = .{ .n = integer, .orginal = tok } } });
        }
        const float = std.fmt.parseFloat(f64, tok.lexeme) catch return error.InvalidNumber;
        return self.allocNode(tree.Node{ .number = .{ .float = .{ .n = float, .orginal = tok } } });
    }
    if (self.consume(.string)) |tok| {
        return self.parseString(tok, false);
    }
    if (self.consume(.character)) |tok| {
        return self.parseString(tok, true);
    }
    if (self.consume(.identifier)) |tok| {
        return self.allocNode(tree.Node{ .identifier = tok });
    }

    if (self.consume(.true_kw)) |tok| {
        return self.allocNode(tree.Node{ .boolean = .{ .n = true, .orginal = tok } });
    }
    if (self.consume(.false_kw)) |tok| {
        return self.allocNode(tree.Node{ .boolean = .{ .n = false, .orginal = tok } });
    }

    if (self.match(.left_bracket)) return self.parseArray();

    return error.ExpectedStatement;
}

fn parseArguments(self: *Self) ParseError![]const *const tree.Node {
    var args = std.ArrayList(*const tree.Node).init(self.node_allocator);
    errdefer args.deinit();
    while (true) {
        const arg = try self.parseExpression();
        try args.append(arg);
        if (self.consume(.comma)) |_| continue;
        break;
    }
    return args.toOwnedSlice();
}

fn parseString(self: *Self, tok: Token, is_char: bool) ParseError!*const tree.Node {
    std.debug.assert(tok.kind == .string or tok.kind == .character);

    const str_raw = tok.lexeme[1 .. tok.lexeme.len - 1]; // remove quotes

    // replace escape characters
    if (str_raw.len < 0) {
        if (is_char) return error.InvalidCharacter;
        return self.allocNode(.{
            .string = .{
                .n = str_raw,
                .orginal = tok,
                .allocated = .stack,
            },
        });
    }

    // check if we have any escape characters
    var i: usize = 0;
    while (i < str_raw.len) : (i += 1) {
        if (str_raw[i] == '\\') {
            break;
        }
    } else {
        if (is_char) {
            if (str_raw.len != 1) return error.InvalidCharacter;
            return self.allocNode(.{
                .char = .{ .n = str_raw[0], .orginal = tok },
            });
        } else {
            return self.allocNode(.{
                .string = .{
                    .n = str_raw,
                    .orginal = tok,
                    .allocated = .stack,
                },
            });
        }
    }

    var buf = self.allocator.alloc(u8, str_raw.len) catch return error.OutOfMemory;
    errdefer self.allocator.free(buf);

    i = 0;
    var j: usize = 0;
    while (i < str_raw.len) : ({
        i += 1;
        j += 1;
    }) {
        if (str_raw[i] == '\\') {
            i += 1;
            if (i >= str_raw.len) return error.MissingEndOfString;
            var char = str_raw[i];
            if (char == 'n') char = '\n';
            if (char == 'r') char = '\r';
            if (char == 't') char = '\t';

            buf[j] = char;
        } else {
            buf[j] = str_raw[i];
        }
    }

    // realloc buffer to fit string aka size = j
    if (is_char) {
        if (buf.len != 1) return error.InvalidCharacter;
        defer self.allocator.free(buf); // no longer need the buffer
        return self.allocNode(tree.Node{ .char = .{ .n = buf[0], .orginal = tok } });
    } else {
        const new_buf = try self.allocator.realloc(buf, j);
        const valid = self.node_allocator.create(bool) catch return error.OutOfMemory;
        valid.* = true;

        return self.allocNode(.{
            .string = .{
                .n = new_buf,
                .orginal = tok,
                .allocated = .{ .heap = .{ .allocator = self.allocator, .valid = valid } },
            },
        });
    }
}

fn parseFunction(self: *Self) ParseError!*const tree.Node {
    const start = self.consume(.func_kw) orelse return self.badToken(error.UnexpectedToken);

    const identifier = self.consume(.identifier) orelse return self.badToken(error.ExpectedIdentifier);

    _ = self.consume(.left_paren) orelse return self.badToken(error.MissingParen);

    if (self.consume(.right_paren)) |_| {
        const ret_type = try self.parseType();
        const body = try self.parseBlock();
        return self.allocNode(tree.Node{
            .function_decl = .{
                .start = start,
                .identifier = identifier,
                .params = &.{},
                .ret_type = ret_type,
                .body = body,
            },
        });
    }

    var params = std.ArrayList(tree.FuncParam).init(self.node_allocator);
    errdefer params.deinit();

    while (true) {
        const t = try self.parseType();
        const name = self.consume(.identifier) orelse return self.badToken(error.ExpectedIdentifier);

        try params.append(.{
            .type = t,
            .name = name,
        });

        if (self.consume(.comma)) |_| {} else break;
    }
    _ = self.consume(.right_paren) orelse return self.badToken(error.MissingParen);

    const ret_type = try self.parseType();
    const body = try self.parseBlock();

    return self.allocNode(tree.Node{
        .function_decl = .{
            .start = start,
            .identifier = identifier,
            .params = try params.toOwnedSlice(),
            .ret_type = ret_type,
            .body = body,
        },
    });
}

fn parseArray(self: *Self) ParseError!*const tree.Node {
    const start = self.consume(.left_bracket) orelse return self.badToken(error.MissingBracket);

    var elements = std.ArrayList(*const tree.Node).init(self.node_allocator);
    errdefer elements.deinit();

    while (self.current() != null and !self.match(.right_bracket)) {
        try elements.append(try self.parseExpression());
        if (self.consume(.comma)) |_| {} else break;
    }

    const end = self.consume(.right_bracket) orelse return self.badToken(error.MissingBracket);

    return self.allocNode(tree.Node{
        .array = .{
            .start = start,
            .items = try elements.toOwnedSlice(),
            .end = end,
        },
    });
}
