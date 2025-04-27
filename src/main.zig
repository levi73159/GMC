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
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} <command>\n", .{args[0]});
        std.process.exit(1);
    }

    if (std.mem.eql(u8, args[1], "repl")) {
        repl(allocator) catch |err| {
            std.debug.print("Error running repl: {s}\n", .{@errorName(err)});
            std.debug.print("Exiting...\n", .{});
            std.process.exit(255);
        };
    } else if (std.mem.eql(u8, args[1], "run")) {
        if (args.len < 3) {
            std.debug.print("Usage: {s} run <file>\n", .{args[0]});
            std.process.exit(1);
        }

        const file = std.fs.cwd().openFile(args[2], .{}) catch |err| {
            std.debug.print("Error opening file: {s}\n", .{@errorName(err)});
            std.debug.print("Exiting...\n", .{});
            std.process.exit(255);
        };
        defer file.close();

        runFile(file) catch |err| {
            std.debug.print("Error running file: {s}\n", .{@errorName(err)});
            std.debug.print("Exiting...\n", .{});
            std.process.exit(255);
        };
    } else if (std.mem.eql(u8, args[1], "help")) {
        std.debug.print("Usage: {s} <command>\n", .{args[0]});
        std.debug.print("Commands:\n", .{});
        std.debug.print("  repl\n", .{});
        std.debug.print("  run <file>\n", .{});
        std.debug.print("  help\n", .{});
    } else {
        std.debug.print("Unknown command: {s}\n", .{args[1]});
        std.process.exit(1);
    }
}

fn repl(gpa: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var symbols = SymbolTable.init(gpa);
    while (true) {
        std.debug.assert(arena.reset(.free_all) == true); // always returns true but we want to be sure

        try stdout.writer().print("> ", .{});
        const text = try stdin.reader().readUntilDelimiterOrEofAlloc(allocator, '\n', 1024) orelse {
            continue;
        };
        defer allocator.free(text);

        run(text, allocator, &symbols);
    }
}

fn runFile(file: std.fs.File) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const contents = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(contents);

    var symbols = SymbolTable.init(allocator);
    defer symbols.deinit();
    run(contents, allocator, &symbols);
}

fn run(text: []const u8, allocator: std.mem.Allocator, symbols: *SymbolTable) void {
    var lexer = Lexer.init(text);
    const tokens = lexer.makeTokens(allocator) catch |err| switch (err) {
        error.OutOfMemory => {
            std.log.err("OUT OF MEMORY!!!", .{});
            std.process.exit(255);
        },
        else => {
            std.debug.print("Lexer error: {s}\n", .{@errorName(err)});
            std.debug.print("{}\n", .{lexer.prev});
            std.debug.print("line: {d}, column: {d}\n", .{ lexer.prev.line, lexer.prev.column });
            return;
        },
    };
    defer allocator.free(tokens);

    var parser = Parser.init(tokens, allocator);
    const nodes = parser.parse() catch |err| switch (err) {
        error.OutOfMemory => {
            std.log.err("OUT OF MEMORY!!!", .{});
            std.process.exit(255);
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
            return;
        },
    };
    defer parser.allocator.free(nodes);

    const interperter = Interpreter.init(symbols);
    interperter.eval(nodes);
}
