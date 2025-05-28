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

allocator: mem.Allocator,
macros: std.StringHashMap([]const Token),

macro_buffer: std.ArrayList(Token), // used for macro expansion or include
macro_buffer_index: usize = 0,

file_contents: std.ArrayList([]const u8),
disable_macro: bool = false,

pub fn init(allocator: mem.Allocator, buffer: []const u8) Self {
    return Self{
        .buffer = buffer,
        .prev = DebugPos{
            .orginal_buffer = buffer,
        },
        .allocator = allocator,
        .macro_buffer = std.ArrayList(Token).init(allocator),
        .file_contents = std.ArrayList([]const u8).init(allocator),
        .macros = std.StringHashMap([]const Token).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.macro_buffer.deinit();
    for (self.file_contents.items) |contents| self.allocator.free(contents);
    self.file_contents.deinit();
    var value_iterator = self.macros.valueIterator();
    while (value_iterator.next()) |tokens| self.allocator.free(tokens.*);
    self.macros.deinit();
}

pub fn next(self: *Self) anyerror!?Token {
    if (self.expandMacro()) |token| return token;

    self.skipWhitespace(); // skip whitespace

    const c = self.advance() orelse return null; // we know the next character is usable because we skipped whitespace
    return switch (c) {
        '(' => self.initToken(.left_paren, "("),
        ')' => self.initToken(.right_paren, ")"),
        '{' => self.initToken(.left_curly_bracket, "{"),
        '}' => self.initToken(.right_curly_bracket, "}"),
        '+' => plus: {
            const start = self.prev;
            if (self.consume('=')) {
                self.prev = start.combine(self.prev);
                break :plus self.initToken(.plus_equal, "+=");
            } else if (self.consume('+')) {
                self.prev = start.combine(self.prev);
                break :plus self.initToken(.plus_plus, "++");
            }
            break :plus self.initToken(.plus, "+");
        },
        '-' => minus: {
            if (self.consume('=')) {
                break :minus self.initToken(.minus_equal, "-=");
            } else if (self.consume('-')) {
                break :minus self.initToken(.minus_minus, "--");
            }
            break :minus self.initToken(.minus, "-");
        },
        '*' => self.initTokenOrOther(.star, .star_equal, "*", "*=", '='),
        '/' => self.initTokenOrOther(.slash, .slash_equal, "/", "/=", '='),
        '%' => self.initToken(.percent, "%"),
        '&' => self.initTokenOrOther(.ampersand, .ampersand_ampersand, "&", "&&", '&'),
        '|' => self.initTokenOrOther(.pipe, .pipe_pipe, "|", "||", '|'),
        '^' => self.initToken(.caret, "^"),
        '<' => self.initTokenOrOther(.lt, .lt_equal, "<", "<=", '='),
        '>' => self.initTokenOrOther(.gt, .gt_equal, ">", ">=", '='),
        ':' => self.initToken(.colon, ":"),
        ';' => self.initToken(.semicolon, ";"),
        '=' => self.initTokenOrOther(.equal, .equal_equal, "=", "==", '='),
        '!' => self.initTokenOrOther(.bang, .bang_equal, "!", "!=", '='),
        ',' => self.initToken(.comma, ","),
        '.' => self.initToken(.dot, "."),
        '0'...'9' => try self.makeNumber(),
        'a'...'z', 'A'...'Z', '_' => try self.makeIdentifier(),
        '\'' => try self.makeString(true),
        '"' => try self.makeString(false),
        '[' => self.initToken(.left_bracket, "["),
        ']' => self.initToken(.right_bracket, "]"),
        '$' => blk: {
            try self.macro();
            break :blk self.next();
        },
        '@' => blk: {
            try self.getExpandMacro();
            break :blk self.next();
        },
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
    _ = token.setPos(self.prev);
    return token;
}

fn initTokenOrOther(self: *Self, kind: Token.Kind, other: Token.Kind, lexeme: []const u8, other_lexeme: []const u8, char: u8) Token {
    var start = self.prev; // save start
    if (self.consume(char)) {
        var token = Token.init(other, other_lexeme);
        _ = token.setPos(start.combine(self.prev));
        return token;
    } else {
        var token = Token.init(kind, lexeme);
        _ = token.setPos(self.prev);
        return token;
    }
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
    if ((self.current() orelse return false) == c) {
        return true;
    }
    return false;
}

fn consume(self: *Self, c: u8) bool {
    if (self.match(c)) {
        _ = self.advance();
        return true;
    }
    return false;
}

// helper functions
fn skipWhitespace(self: *Self) void {
    while (true) {
        const c = self.current() orelse return;

        if (std.ascii.isWhitespace(c)) {
            _ = self.advance();
        } else if (c == '#') {
            // Skip to end of line or end of file
            while (self.current()) |ch| {
                _ = self.advance();
                if (ch == '\n') break;
            }
        } else {
            return;
        }
    }
}

fn isNumber(c: u8) bool {
    return (c >= '0' and c <= '9') or c == '.' or c == '_';
}

fn isHex(c: u8) bool {
    return std.ascii.isHex(c) or c == '_';
}

fn isBinary(c: u8) bool {
    return c == '0' or c == '1' or c == '_';
}

fn makeNumber(self: *Self) !Token {
    const start = self.index - 1;
    const start_column = self.column - 1;

    blk: {
        const base_prefix = self.current() orelse break :blk;
        if (base_prefix == 'x' or base_prefix == 'X') {
            _ = self.advance();
            while (isHex(self.current() orelse break :blk)) {
                _ = self.advance();
            }
        } else if (base_prefix == 'b' or base_prefix == 'B') {
            _ = self.advance();
            while (isBinary(self.current() orelse break :blk)) {
                _ = self.advance();
            }
        } else {
            while (isNumber(self.current() orelse break :blk)) {
                _ = self.advance();
            }
        }
    }

    const end = self.index;
    const lexeme = self.buffer[start..end];
    const debug_pos = DebugPos{
        .start = start,
        .end = end,
        .orginal_buffer = self.buffer,
        .line = self.line,
        .column = start_column,
    };
    self.prev = debug_pos;
    if (mem.count(u8, lexeme, ".") > 1) {
        return error.InvalidNumber;
    }
    var tok = Token.init(.number, lexeme);
    _ = tok.setPos(debug_pos);
    return tok;
}

fn isIdentifier(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_';
}

fn makeIdentifier(self: *Self) !Token {
    // check if it is a type then if not then it is an identifier

    const start = self.index - 1;
    const start_column = self.column - 1;

    blk: {
        while (isIdentifier(self.current() orelse break :blk)) {
            _ = self.advance();
        }
    }

    const end = self.index;
    const lexeme = self.buffer[start..end];
    const debug_pos = DebugPos{
        .start = start,
        .end = end,
        .orginal_buffer = self.buffer,
        .line = self.line,
        .column = start_column,
    };

    self.prev = debug_pos;

    if (isType(lexeme)) |value| {
        var tok = Token.init(.type, lexeme);
        _ = tok.setPos(debug_pos).setValue(value);
        return tok;
    } else if (Token.Kind.getKeyword(lexeme)) |kind| {
        var tok = Token.init(kind, lexeme);
        _ = tok.setPos(debug_pos);
        return tok;
    } else {
        var tok = Token.init(.identifier, lexeme);
        _ = tok.setPos(debug_pos);
        return tok;
    }
}

fn makeString(self: *Self, is_char: bool) !Token {
    const start = self.index - 1;
    const start_column = self.column - 1;
    const start_line = self.line;

    const end_char: u8 = if (is_char) '\'' else '"';
    while (self.current() != end_char) {
        if (self.current() == null) return error.UnexpectedEOF;
        const c = self.advance();
        if (c == '\\') _ = self.advance();
    }
    const c = self.advance();
    const end = self.index;

    if (c == null) return error.UnexpectedEOF;
    if (c != end_char) return error.ExpectedEndOfString;

    const lexeme = self.buffer[start..end];
    const prev = DebugPos{
        .start = start,
        .end = end,
        .orginal_buffer = self.buffer,
        .line = self.line,
        .column = start_column,
        .multi_line = self.line != start_line,
    };
    self.prev = prev;

    if (prev.multi_line) return error.MultiLineStringNotSupported;

    var tok = if (is_char) Token.init(.character, lexeme) else Token.init(.string, lexeme);
    _ = tok.setPos(prev);
    return tok;
}

fn isType(lexeme: []const u8) ?Token.Value {
    const type_value = std.meta.stringToEnum(Token.TypeValue, lexeme);
    if (type_value) |value| {
        return Token.Value{ .type = value };
    }
    return null;
}

fn macro(self: *Self) !void {
    if (self.disable_macro) return error.MacroDisabled;
    const macro_ident = try self.next() orelse return error.UnexpectedEOF;
    if (macro_ident.kind != .identifier) return error.ExpectedMacroIdentifier;

    if (std.mem.eql(u8, macro_ident.lexeme, "include")) {
        try self.includeMacro();
    } else if (std.mem.eql(u8, macro_ident.lexeme, "define")) {
        try self.defineMacro();
    } else {
        return error.UnknownMacroCommand;
    }
}

fn includeMacro(self: *Self) !void {
    const include_path_str = try self.next() orelse return error.UnexpectedEOF;
    if (include_path_str.kind != .string) return error.ExpectedIncludePath;

    const include_path = include_path_str.lexeme[1 .. include_path_str.lexeme.len - 1];

    const file = try std.fs.cwd().openFile(include_path, .{});
    defer file.close();

    const contents = try file.readToEndAlloc(self.allocator, std.math.maxInt(usize));
    self.file_contents.append(contents) catch unreachable;

    var lexer = Self.init(self.allocator, contents);
    while (try lexer.next()) |token| {
        try self.macro_buffer.append(token);
    }
}

fn defineMacro(self: *Self) !void {
    const macro_name = try self.next() orelse return error.UnexpectedEOF;
    if (macro_name.kind != .identifier) return error.ExpectedMacroIdentifier;
    var semi_colon_count: u8 = 0; // if we find two semicolons in a row we are done
    var last_token: ?Token = null;

    var macro_tokens = std.ArrayList(Token).init(self.allocator);
    errdefer macro_tokens.deinit();

    // make until end of line
    while (try self.next()) |token| {
        defer last_token = token;
        if (token.kind == .semicolon) {
            semi_colon_count += 1;
            if (semi_colon_count == 2) break;
            continue;
        }
        if (semi_colon_count == 1 and token.kind != .semicolon) {
            semi_colon_count = 0;
            try macro_tokens.append(last_token.?);
        }
        try macro_tokens.append(token);
    }

    self.macros.put(macro_name.lexeme, try macro_tokens.toOwnedSlice()) catch unreachable;
}

fn expandMacro(self: *Self) ?Token {
    if (self.disable_macro) return null;
    if (self.macro_buffer.items.len == 0) return null;
    if (self.macro_buffer_index >= self.macro_buffer.items.len) return null;
    const token = self.macro_buffer.items[self.macro_buffer_index];
    self.macro_buffer_index += 1;
    return token;
}

fn getExpandMacro(self: *Self) !void {
    if (self.disable_macro) return error.MacroDisabled;
    const macro_name = try self.next() orelse return error.UnexpectedEOF;
    if (macro_name.kind != .identifier) return error.ExpectedMacroIdentifier;

    if (self.macros.get(macro_name.lexeme)) |tokens| {
        var i: usize = tokens.len - 1;
        if (self.macro_buffer.items.len == 0) {
            try self.macro_buffer.appendSlice(tokens);
        } else {
            while (i > 0) : (i -= 1) {
                try self.macro_buffer.insert(self.macro_buffer_index + 1, tokens[i]);
            }
        }
    }
}
