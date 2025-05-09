const std = @import("std");
const DebugPos = @import("DebugPos.zig");

const Self = @This();

pub const Kind = enum {
    // Basic Tokens
    left_paren,
    right_paren,
    left_curly_bracket,
    right_curly_bracket,
    left_bracket,
    right_bracket,

    plus,
    minus,
    star,
    slash,
    percent,
    ampersand,
    ampersand_ampersand,
    pipe,
    pipe_pipe,
    caret,
    lt,
    gt,
    lt_lt, // <<
    gt_gt, // >>
    lt_equal,
    gt_equal,
    equal_equal,
    bang,
    bang_equal,
    comma,

    semicolon,
    equal,

    plus_equal,
    minus_equal,
    star_equal,
    slash_equal,

    plus_plus,
    minus_minus,

    // literal tokens
    number,
    string,
    character,
    identifier,
    type,

    // keywords
    var_kw,
    const_kw,
    func_kw,
    true_kw,
    false_kw,
    if_kw,
    else_kw,
    for_kw,
    while_kw,
    break_kw,
    continue_kw,
    return_kw,

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

    int,
    float,
    bool,
    void,
    str,
    char,

    // objects?
    // list is mutable and imlist is immutable hince [im]mutable
    list,
    imlist,
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
