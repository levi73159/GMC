const std = @import("std");
const DebugPos = @import("DebugPos.zig");

const Self = @This();

pub const Kind = enum {
    // single character tokens
    left_paren,
    right_paren,
    plus,
    minus,
    star,
    slash,

    // literal tokens
    number,
};

kind: Kind,
lexeme: []const u8,
pos: ?DebugPos = null, // a null value means the token has no position aka it is invalid or inserted

pub fn init(kind: Kind, lexeme: []const u8) Self {
    return Self{
        .kind = kind,
        .lexeme = lexeme,
    };
}

pub fn setPos(self: *Self, pos: DebugPos) void {
    self.pos = pos;
}
