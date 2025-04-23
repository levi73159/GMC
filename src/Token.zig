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
    semicolon,
    equal,

    // multi character tokens
    plus_equal,
    minus_equal,
    star_equal,
    slash_equal,

    plus_plus,
    minus_minus,

    // literal tokens
    number,
    identifier,
    type,

    // keywords
    var_kw,
    const_kw,

    pub fn getKeyword(lexeme: []const u8) ?Kind {
        var buf: [2555]u8 = undefined;
        const name = std.fmt.bufPrint(&buf, "{s}_kw", .{lexeme}) catch unreachable;
        return std.meta.stringToEnum(Kind, name);
    }
};

pub const TypeValue = enum {
    i8,
    i16,
    i32,
    i64,
    u8,
    u16,
    u32,
    u64,
    f32,
    f64,
};

pub const Value = union(enum) {
    none: void,
    type: TypeValue,
};

kind: Kind,
value: Value = .none,
lexeme: []const u8,
pos: ?DebugPos = null, // a null value means the token has no position aka it is invalid or inserted

pub fn init(kind: Kind, lexeme: []const u8) Self {
    return Self{
        .kind = kind,
        .lexeme = lexeme,
    };
}

pub fn setPos(self: *Self, pos: ?DebugPos) *Self {
    self.pos = pos;
    return self;
}

pub fn setValue(self: *Self, value: Value) *Self {
    self.value = value;
    return self;
}
