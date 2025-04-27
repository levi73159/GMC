const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Interpreter = @import("Interpreter.zig");
const SymbolTable = @import("SymbolTable.zig");
const Token = @import("Token.zig");
const Node = @import("tree.zig").Node;

fn printTokens(tokens: []const Token) void {
    for (tokens) |token| {
        std.debug.print("{s} ({s})\n", .{ token.lexeme, @tagName(token.kind) });
    }
}

pub fn tokenToString(token: Token) []const u8 {
    return token.lexeme;
}

pub fn printBlock(block: []const *Node, indent: usize) void {
    for (block) |node| {
        prettyPrint(node, indent);
    }
}

pub fn prettyPrint(node: *const Node, indent: usize) void {
    const stdout = std.io.getStdOut().writer();

    for (0..indent) |_| stdout.print("  ", .{}) catch return;

    switch (node.*) {
        .number => |num| switch (num) {
            .integer => |i| stdout.print("Integer({})\n", .{i.n}) catch return,
            .float => |f| stdout.print("Float({d})\n", .{f.n}) catch return,
        },
        .boolean => |b| stdout.print("Boolean({})\n", .{b.n}) catch return,
        .block => |b| {
            stdout.print("Block\n", .{}) catch return;
            printBlock(b.nodes, indent + 1);
        },
        .bin_op => |op| {
            stdout.print("BinOp({s})\n", .{tokenToString(op.op)}) catch return;
            prettyPrint(op.left, indent + 1);
            prettyPrint(op.right, indent + 1);
        },
        .unary_op => |u| {
            stdout.print("UnaryOp({s})\n", .{tokenToString(u.op)}) catch return;
            prettyPrint(u.right, indent + 1);
        },
        .identifier => |i| stdout.print("Identifier({s})\n", .{i.lexeme}) catch return,
        .var_decl => |v| {
            stdout.print("VarDecl({s}, const={})", .{ v.identifier.lexeme, v.is_const.n }) catch return;
            stdout.print(" Type({s})\n", .{@tagName(v.type.value.type)}) catch return;
            if (v.value) |value| {
                prettyPrint(value, indent + 1);
            }
        },
        .var_assign => |v| {
            stdout.print("VarAssign({s})\n", .{v.identifier.lexeme}) catch return;
            prettyPrint(v.value, indent + 1);
        },
        .ifstmt => |i| {
            stdout.print("IfStmt\n", .{}) catch return;
            prettyPrint(i.condition, indent + 1);
            prettyPrint(i.then, indent + 1);
            if (i.else_node) |else_block| {
                prettyPrint(else_block, indent + 1);
            }
        },
    }
}

pub fn main() !void {
    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var symbols = SymbolTable.init(gpa.allocator());
    while (true) {
        std.debug.assert(arena.reset(.free_all) == true); // always returns true but we want to be sure

        try stdout.writer().print("> ", .{});
        const text = try stdin.reader().readUntilDelimiterOrEofAlloc(allocator, '\n', 1024) orelse {
            continue;
        };
        defer allocator.free(text);

        var lexer = Lexer.init(text);
        const tokens = lexer.makeTokens(allocator) catch |err| switch (err) {
            error.OutOfMemory => {
                std.log.err("OUT OF MEMORY!!!", .{});
                std.process.exit(1);
            },
            else => {
                std.debug.print("Lexer error: {s}\n", .{@errorName(err)});
                std.debug.print("{}\n", .{lexer.prev});
                std.debug.print("line: {d}, column: {d}\n", .{ lexer.prev.line, lexer.prev.column });
                continue;
            },
        };
        defer allocator.free(tokens);

        var parser = Parser.init(tokens, allocator);
        const nodes = parser.parse() catch |err| switch (err) {
            error.OutOfMemory => {
                std.log.err("OUT OF MEMORY!!!", .{});
                std.process.exit(1);
            },
            else => {
                const mprev_tok = parser.previous();
                std.debug.print("Parser error: {s}\n", .{@errorName(err)});
                if (mprev_tok) |prev_tok| {
                    std.debug.print("{}\n", .{prev_tok.pos.?});
                    std.debug.print("line: {d}, column: {d}\n", .{ prev_tok.pos.?.line, prev_tok.pos.?.column });
                } else if (parser.prev) |prev| {
                    std.debug.print("{}\n", .{prev.pos.?});
                    std.debug.print("line: {d}, column: {d}\n", .{ prev.pos.?.line, prev.pos.?.column });
                } else if (parser.tokens.len > 0) {
                    const first = parser.tokens[0];
                    std.debug.print("{}\n", .{first.pos.?});
                    std.debug.print("line: {d}, column: {d}\n", .{ first.pos.?.line, first.pos.?.column });
                } else {
                    std.debug.print("Position cannot be determined\n", .{});
                }
                continue;
            },
        };
        defer parser.allocator.free(nodes);

        const interperter = Interpreter.init(&symbols);
        interperter.eval(nodes);
    }
}
