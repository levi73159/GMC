const std = @import("std");
const mem = std.mem;
const Token = @import("Token.zig");
const DebugPos = @import("DebugPos.zig");

const Self = @This();

buffer: []const u8,
index: usize = 0,
line: u32 = 1,
column: u32 = 1,
prev: DebugPos = DebugPos{
    .orginal_buffer = undefined,
},

pub fn init(buffer: []const u8) Self {
    return Self{ .buffer = buffer, .prev = DebugPos{
        .orginal_buffer = buffer,
    } };
}

pub fn next(self: *Self) !?Token {
    self.skipWhitespace(); // skip whitespace

    const c = self.advance() orelse return null; // we know the next character is usable because we skipped whitespace
    return switch (c) {
        '(' => self.initToken(.left_paren, "("),
        ')' => self.initToken(.right_paren, ")"),
        '+' => self.initToken(.plus, "+"),
        '-' => self.initToken(.minus, "-"),
        '*' => self.initToken(.star, "*"),
        '/' => self.initToken(.slash, "/"),
        '0'...'9' => try self.makeNumber(),
        else => return error.InvalidCharacter,
    };
}

pub fn makeTokens(self: *Self, allocator: mem.Allocator) ![]const Token {
    var tokens = std.ArrayList(Token).init(allocator);
    errdefer tokens.deinit();

    while (try self.next()) |token| {
        try tokens.append(token);
    }

    return tokens.toOwnedSlice();
}

fn initToken(self: *Self, kind: Token.Kind, lexeme: []const u8) Token {
    var token = Token.init(kind, lexeme);
    token.setPos(self.prev);
    return token;
}

fn get(self: Self, i: usize) ?u8 {
    if (i < self.buffer.len) {
        return self.buffer[i];
    }
    return null;
}

// return the token and advance
fn advance(self: *Self) ?u8 {
    var char = self.get(self.index);

    self.prev.start = self.index;
    self.prev.end = self.index + 1; // because [1..1] is not vaild but [1..2] is because it have one char
    self.prev.line = self.line;
    self.prev.column = self.column;

    self.index += 1;
    self.column += 1;
    // deal with windows newlines
    if (char == '\r') {
        self.index += 2; // skip \r\n
        self.line += 1;
        self.column = 1;
        char = self.get(self.index); // replace \r with \n
        std.debug.assert(self.get(self.index - 1).? == '\n');
    } else if (char == '\n') {
        self.line += 1;
        self.column = 1;
    }
    return char;
}

fn current(self: Self) ?u8 {
    return self.get(self.index);
}

fn peek(self: Self) ?u8 {
    return self.peekN(1);
}

fn peekN(self: Self, n: usize) ?u8 {
    return self.get(self.index + n);
}

fn match(self: *Self, c: u8) bool {
    if (self.current() orelse return false == c) {
        self.index += 1;
        return true;
    }
    return false;
}

// helper functions
fn skipWhitespace(self: *Self) void {
    while (std.ascii.isWhitespace(self.current() orelse return)) {
        _ = self.advance();
    }
}

fn isNumber(c: u8) bool {
    return (c >= '0' and c <= '9') or c == '.' or c == '_';
}

fn makeNumber(self: *Self) !Token {
    const start = self.index - 1;

    blk: {
        while (isNumber(self.current() orelse break :blk)) {
            _ = self.advance();
        }
    }

    const end = self.index;
    const lexeme = self.buffer[start..end];
    var tok = Token.init(.number, lexeme);
    tok.setPos(.{
        .start = start,
        .end = end,
        .orginal_buffer = self.buffer,
        .line = self.line,
        .column = self.column,
    });
    return tok;
}
