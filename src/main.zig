const std = @import("std");
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");

fn printTokens(tokens: []const Token) void {
    for (tokens) |token| {
        std.debug.print("{s} ({s})\n", .{ token.lexeme, @tagName(token.kind) });
    }
}

pub fn main() !void {
    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    while (true) {
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
        printTokens(tokens);
    }
}
