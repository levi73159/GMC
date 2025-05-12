const std = @import("std");
const Token = @import("Token.zig");
const DebugPos = @import("DebugPos.zig");
const Type = @import("Type.zig");

pub fn BaseType(comptime T: type) type {
    return struct {
        n: T,
        orginal: Token,

        pub fn deinit(_: @This()) void {}
    };
}

pub const Node = union(enum) {
    number: Number,
    string: String,
    char: BaseType(u8),
    boolean: BaseType(bool),
    block: Block,
    identifier: Token,
    bin_op: BinOp,
    unary_op: UnaryOp,
    var_decl: VariableDecl,
    var_assign: VariableAssign,
    ifstmt: IfStmt,
    forstmt: ForStmt,
    breakstmt: BreakStmt, // only need a token for debug pos
    continuestmt: Token,
    whilestmt: WhileStmt,
    function_decl: FunctionDecl,
    call: Call,
    returnstmt: ReturnStmt,
    array: Array,
    index_access: IndexAccess,
    field_access: FieldAccess,
    includestmt: IncludeStmt,

    pub fn getLeftPos(self: Node) ?DebugPos {
        return switch (self) {
            .number => |n| switch (n) {
                .integer => |i| i.orginal.pos,
                .float => |f| f.orginal.pos,
            },
            .string => |s| s.orginal.pos,
            .char => |c| c.orginal.pos,
            .boolean => |b| b.orginal.pos,
            .block => |b| b.start.pos,
            .identifier => |i| i.pos,
            .bin_op => |b| b.left.getLeftPos(),
            .unary_op => |u| u.op.pos,
            .var_decl => |v| v.is_const.orginal.pos,
            .var_assign => |v| v.left.getLeftPos(),
            .ifstmt => |i| i.start.pos,
            .forstmt => |f| f.start.pos,
            .breakstmt => |b| b.start.pos,
            .continuestmt => |c| c.pos,
            .whilestmt => |w| w.start.pos,
            .function_decl => |f| f.start.pos,
            .call => |c| c.callee.getLeftPos(),
            .returnstmt => |r| r.start.pos,
            .array => |a| a.start.pos,
            .index_access => |i| i.value.getLeftPos(),
            .field_access => |f| f.value.getLeftPos(),
            .includestmt => |i| i.start.pos,
        };
    }

    pub fn getRightPos(self: Node) ?DebugPos {
        return switch (self) {
            .number => |n| switch (n) {
                .integer => |i| i.orginal.pos,
                .float => |f| f.orginal.pos,
            },
            .string => |s| s.orginal.pos,
            .char => |c| c.orginal.pos,
            .boolean => |b| b.orginal.pos,
            .block => |b| b.end.pos,
            .identifier => |i| i.pos,
            .bin_op => |b| b.right.getRightPos(),
            .unary_op => |u| u.right.getRightPos(),
            .var_decl => |v| if (v.value) |value| value.getRightPos() else v.identifier.pos,
            .var_assign => |v| v.value.getRightPos(),
            .ifstmt => |i| if (i.else_node) |else_node| else_node.getRightPos() else i.then.getRightPos(),
            .forstmt => |f| if (f.else_node) |else_node| else_node.getRightPos() else f.body.getRightPos(),
            .breakstmt => |b| if (b.value) |value| value.getRightPos() else b.start.pos,
            .continuestmt => |c| c.pos,
            .whilestmt => |w| w.body.getRightPos(),
            .function_decl => |f| f.body.getRightPos(),
            .call => |c| c.end.pos,
            .returnstmt => |r| if (r.value) |value| value.getRightPos() else r.start.pos,
            .array => |a| a.end.pos,
            .index_access => |i| i.value.getRightPos(),
            .field_access => |f| f.field.pos,
            .includestmt => |i| i.path.pos,
        };
    }

    pub fn getPos(self: Node) ?DebugPos {
        const left = self.getLeftPos() orelse return null;
        const right = self.getRightPos() orelse return null;
        return left.combine(right);
    }

    pub fn deinit(self: Node) void {
        const T = @TypeOf(self);
        const tag = std.meta.activeTag(self); // Get current union tag

        inline for (@typeInfo(T).@"union".fields) |field| {
            if (std.mem.eql(u8, field.name, @tagName(tag))) {
                if (@hasDecl(field.type, "deinit")) {
                    @field(self, field.name).deinit();
                }
            }
        }
    }
};

// node infos are stored in structs and in the order they come in
// so for var u8 x = 32; the first thing will come is wheter is const or not and then finally the node value;
pub const Number = union(enum) {
    integer: BaseType(i65),
    float: BaseType(f64),

    pub fn deinit(_: Number) void {}
};

pub const String = struct {
    n: []const u8,
    orginal: Token,
    allocated: union(enum) {
        heap: struct { allocator: std.mem.Allocator, valid: *bool },
        stack: void,
    },

    pub fn deinit(self: String) void {
        switch (self.allocated) {
            .heap => |h| {
                if (h.valid.*) {
                    h.allocator.free(self.n);
                    h.valid.* = false;
                }
            },
            .stack => {},
        }
    }
};

pub const Block = struct {
    start: Token,
    nodes: []const *const Node,
    end: Token,

    pub fn deinit(self: Block) void {
        for (self.nodes) |node| node.deinit();
    }
};

pub const BinOp = struct {
    left: *const Node,
    op: Token,
    right: *const Node,

    pub fn deinit(self: BinOp) void {
        self.left.deinit();
        self.right.deinit();
    }
};

pub const UnaryOp = struct {
    op: Token,
    right: *const Node,

    pub fn deinit(self: UnaryOp) void {
        self.right.deinit();
    }
};

pub const VariableDecl = struct {
    is_const: BaseType(bool),
    type: Type,
    identifier: Token,
    value: ?*const Node,

    pub fn deinit(self: VariableDecl) void {
        if (self.value) |value| value.deinit();
    }
};

pub const VariableAssign = struct {
    left: *const Node,
    value: *const Node,

    pub fn deinit(self: VariableAssign) void {
        self.left.deinit();
        self.value.deinit();
    }
};

fn blocklessCheck(node: Node) bool {
    return switch (node) {
        .ifstmt => |i| i.isBlockless(),
        .forstmt => |f| f.isBlockless(),
        .whilestmt => |w| w.isBlockless(),
        .block => false,
        else => true,
    };
}

pub const IfStmt = struct {
    start: Token, // aka the if keyword token
    condition: *const Node,
    then: *const Node, // block or single expression
    else_node: ?*const Node,

    pub fn isBlockless(self: IfStmt) bool {
        if (self.else_node) |else_node| {
            return blocklessCheck(else_node.*);
        }
        return blocklessCheck(self.then.*);
    }

    pub fn deinit(self: IfStmt) void {
        self.condition.deinit();
        self.then.deinit();
        if (self.else_node) |else_node| else_node.deinit();
    }
};

pub const ForStmt = struct {
    start: Token,
    start_statement: *const Node,
    condition: *const Node,
    every_iteration: *const Node,
    body: *const Node,
    else_node: ?*const Node,

    pub fn isBlockless(self: ForStmt) bool {
        if (self.else_node) |else_node| {
            return blocklessCheck(else_node.*);
        }
        return blocklessCheck(self.body.*);
    }

    pub fn deinit(self: ForStmt) void {
        self.start_statement.deinit();
        self.condition.deinit();
        self.every_iteration.deinit();
        self.body.deinit();
        if (self.else_node) |else_node| else_node.deinit();
    }
};

pub const BreakStmt = struct {
    start: Token,
    value: ?*const Node,

    pub fn deinit(self: BreakStmt) void {
        if (self.value) |value| value.deinit();
    }
};

pub const WhileStmt = struct {
    start: Token,
    condition: *const Node,
    body: *const Node,

    pub fn isBlockless(self: WhileStmt) bool {
        return blocklessCheck(self.body.*);
    }

    pub fn deinit(self: WhileStmt) void {
        self.condition.deinit();
        self.body.deinit();
    }
};

pub const FuncParam = struct {
    type: Token, // should be an type
    name: Token, // should be an identifier
};

pub const FunctionDecl = struct {
    start: Token,
    identifier: Token,
    params: []const FuncParam,
    ret_type: Token,
    body: *const Node,

    pub fn deinit(self: FunctionDecl) void {
        self.body.deinit();
    }
};

pub const Call = struct {
    callee: *const Node,
    args: []const *const Node,
    end: Token,

    pub fn deinit(self: Call) void {
        for (self.args) |arg| arg.deinit();
    }
};

pub const ReturnStmt = struct {
    start: Token,
    value: ?*const Node,

    pub fn deinit(self: ReturnStmt) void {
        if (self.value) |value| value.deinit();
    }
};

pub const Array = struct {
    start: Token,
    items: []const *const Node,
    end: Token,

    pub fn deinit(self: Array) void {
        for (self.items) |element| element.deinit();
    }
};

pub const IndexAccess = struct {
    value: *const Node,
    index: *const Node,

    pub fn deinit(self: IndexAccess) void {
        self.value.deinit();
        self.index.deinit();
    }
};

pub const FieldAccess = struct {
    value: *const Node,
    field: Token,

    pub fn deinit(self: FieldAccess) void {
        self.value.deinit();
    }
};

pub const IncludeStmt = struct {
    start: Token,
    path: Token,
};
