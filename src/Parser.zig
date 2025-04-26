const std = @import("std");
const Token = @import("Token.zig");
const tree = @import("tree.zig");

const Self = @This();

tokens: []const Token,
index: usize = 0,
prev: ?Token = null,
allocator: std.mem.Allocator,

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
    NumberOverflow,
    InvalidNumber,
};

pub fn init(tokens: []const Token, allocator: std.mem.Allocator) Self {
    return Self{ .tokens = tokens, .index = 0, .allocator = allocator };
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
    self.index += 1;
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

fn allocNode(self: *Self, node: tree.Node) !*tree.Node {
    const ptr = try self.allocator.create(tree.Node);
    ptr.* = node;
    return ptr;
}

pub fn parse(self: *Self) ParseError![]const *tree.Node {
    var nodes: std.ArrayList(*tree.Node) = std.ArrayList(*tree.Node).init(self.allocator);
    errdefer nodes.deinit();
    while (try self.parseInstruction()) |node| {
        try nodes.append(node);
    }
    return nodes.toOwnedSlice();
}

pub fn parseInstruction(self: *Self) ParseError!?*tree.Node {
    if (self.current() == null) return null;
    return self.parseBlock(&parseStatement); // if no block parse statement
}

const ParseFn = *const fn (*Self) ParseError!*tree.Node;
fn parseBinaryOperand(self: *Self, operators: []const Token.Kind, next: ParseFn) ParseError!*tree.Node {
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

fn parseBlock(self: *Self, next: ParseFn) ParseError!*tree.Node {
    if (self.consume(.left_curly_bracket)) |start| {
        var nodes: std.ArrayList(*tree.Node) = std.ArrayList(*tree.Node).init(self.allocator);
        errdefer {
            for (nodes.items) |node| self.allocator.destroy(node);
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

    return next(self);
}

fn parseStatement(self: *Self) ParseError!*tree.Node {
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

fn parseExpression(self: *Self) ParseError!*tree.Node {
    if (self.match(.var_kw) or self.match(.const_kw)) {
        return self.parseVariableDecl();
    }
    if (self.match(.identifier)) {
        if (self.matchP(.equal, 1)) {
            return self.parseVariableAssign();
        }
        // zig fmt: off
        if (self.matchP(.plus_equal, 1) 
            or self.matchP(.minus_equal, 1) 
            or self.matchP(.star_equal, 1) 
            or self.matchP(.slash_equal, 1) 
            or self.matchP(.plus_plus, 1) 
            or self.matchP(.minus_minus, 1)) {
            return self.parseVariableAssignOp();
        }
        // zig fmt: on
    }
    // use match instead of consume because the parseIf checks for the if keyword
    if (self.match(.if_kw)) return self.parseIf(true);

    return self.parseLogicalOr();
}

fn parseVariableDecl(self: *Self) ParseError!*tree.Node {
    const tok = self.advance().?; // should never be null
    const is_const = tok.kind == .const_kw;
    const t = self.consume(.type) orelse return self.badToken(error.ExpectedType);
    const identifier = self.consume(.identifier) orelse return self.badToken(error.ExpectedIdentifier);
    // value can be null
    if (self.consume(.equal)) |_| {
        const value = try self.parseExpression(); // so we can do var u8 x = var u8 y = 10;
        return self.allocNode(tree.Node{
            .var_decl = .{
                .is_const = .{ .n = is_const, .orginal = tok },
                .type = t,
                .identifier = identifier,
                .value = value,
            },
        });
    } else {
        return self.allocNode(tree.Node{
            .var_decl = .{
                .is_const = .{ .n = is_const, .orginal = tok },
                .type = t,
                .identifier = identifier,
                .value = null,
            },
        });
    }
}

fn parseVariableAssign(self: *Self) ParseError!*tree.Node {
    const identifier = self.consume(.identifier) orelse return self.badToken(error.ExpectedIdentifier);
    _ = self.consume(.equal) orelse return self.badToken(error.ExpectedEqual);
    const value = try self.parseExpression();
    return self.allocNode(tree.Node{ .var_assign = .{ .identifier = identifier, .value = value } });
}

fn parseVariableAssignOp(self: *Self) ParseError!*tree.Node {
    const identifier = self.consume(.identifier) orelse return self.badToken(error.ExpectedIdentifier);
    const op = self.advance() orelse return error.MissingOperator;

    const value = blk: {
        if (op.kind == .plus_plus or op.kind == .minus_minus) {
            var new_tok = Token.init(.number, "1");
            _ = new_tok.setPos(op.pos);
            break :blk try self.allocNode(tree.Node{ .number = .{ .integer = .{ .n = 1, .orginal = new_tok } } }); // We create a token with pos == null
        } else {
            break :blk try self.parseExpression();
        }
    };

    const new_op_kind: Token.Kind = switch (op.kind) {
        .plus_plus => .plus,
        .minus_minus => .minus,
        .plus_equal => .plus,
        .minus_equal => .minus,
        .star_equal => .star,
        .slash_equal => .slash,
        else => return error.InvalidToken,
    };

    var new_op = Token.init(new_op_kind, op.lexeme);
    _ = new_op.setPos(op.pos).setValue(op.value);

    return self.allocNode(.{
        .var_assign = .{
            .identifier = identifier,
            .value = try self.allocNode(tree.Node{
                .bin_op = .{
                    .left = try self.allocNode(tree.Node{ .identifier = identifier }),
                    .op = new_op,
                    .right = value,
                },
            }),
        },
    });
}

fn parseIf(self: *Self, expect_value: bool) ParseError!*tree.Node {
    const start = self.consume(.if_kw) orelse return self.badToken(error.UnexpectedToken); // should not happen so we don't make the error clear
    _ = self.consume(.left_paren) orelse return self.badToken(error.MissingParen);
    // parse the condition (expression)
    const condition = try self.parseExpression();
    _ = self.consume(.right_paren) orelse return self.badToken(error.MissingParen);

    // const then = try if (self.match(.left_curly_bracket)) self.parseBlock() else self.parseExpression();
    const then = try self.parseBlock(&parseExpression); // because we don't want to parse a statement (aka expr with semicolon)
    // if this was parse statement it will look like this
    // const u8 x = if (true) 10;; because were already parsing a statement
    if (expect_value) { // gurantee we have an else
        if (!self.match(.if_kw) and !self.match(.else_kw)) {
            return self.badToken(error.ExpectedElse);
        }
    }

    if (self.consume(.else_kw)) |_| {
        const else_node = try self.parseBlock(&parseExpression);
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

fn parseLogicalOr(self: *Self) ParseError!*tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{.pipe_pipe}, &parseLogicalAnd);
}

fn parseLogicalAnd(self: *Self) ParseError!*tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{.ampersand_ampersand}, &parseBitor);
}

fn parseBitor(self: *Self) ParseError!*tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{.pipe}, &parseXor);
}

fn parseXor(self: *Self) ParseError!*tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{.caret}, &parseBitand);
}

fn parseBitand(self: *Self) ParseError!*tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{.ampersand}, &parseEquality);
}

fn parseEquality(self: *Self) ParseError!*tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{ .equal_equal, .bang_equal }, &parseRelational);
}

fn parseRelational(self: *Self) ParseError!*tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{ .lt, .gt, .lt_equal, .gt_equal }, &parseShift);
}

fn parseShift(self: *Self) ParseError!*tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{ .lt_lt, .gt_gt }, &parseTerm);
}

fn parseTerm(self: *Self) ParseError!*tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{ .plus, .minus }, &parseFactor);
}

fn parseFactor(self: *Self) ParseError!*tree.Node {
    return self.parseBinaryOperand(&[_]Token.Kind{ .star, .slash, .percent }, &parseUnary);
}

fn parseUnary(self: *Self) ParseError!*tree.Node {
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

fn parsePrimary(self: *Self) ParseError!*tree.Node {
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
    if (self.consume(.identifier)) |tok| {
        return self.allocNode(tree.Node{ .identifier = tok });
    }

    if (self.consume(.true_kw)) |tok| {
        return self.allocNode(tree.Node{ .boolean = .{ .n = true, .orginal = tok } });
    }
    if (self.consume(.false_kw)) |tok| {
        return self.allocNode(tree.Node{ .boolean = .{ .n = false, .orginal = tok } });
    }

    return error.ExpectedStatement;
}
