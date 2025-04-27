const Token = @import("Token.zig");
const DebugPos = @import("DebugPos.zig");

pub fn BaseType(comptime T: type) type {
    return struct {
        n: T,
        orginal: Token,
    };
}

pub const Node = union(enum) {
    number: Number,
    boolean: BaseType(bool),
    block: Block,
    identifier: Token,
    bin_op: BinOp,
    unary_op: UnaryOp,
    var_decl: VariableDecl,
    var_assign: VariableAssign,
    ifstmt: IfStmt,

    pub fn getLeftPos(self: Node) ?DebugPos {
        return switch (self) {
            .number => |n| switch (n) {
                .integer => |i| i.orginal.pos,
                .float => |f| f.orginal.pos,
            },
            .boolean => |b| b.orginal.pos,
            .block => |b| b.start.pos,
            .identifier => |i| i.pos,
            .bin_op => |b| b.left.getLeftPos(),
            .unary_op => |u| u.op.pos,
            .var_decl => |v| v.is_const.orginal.pos,
            .var_assign => |v| v.identifier.pos,
            .ifstmt => |i| i.start.pos,
        };
    }

    pub fn getRightPos(self: Node) ?DebugPos {
        return switch (self) {
            .number => |n| switch (n) {
                .integer => |i| i.orginal.pos,
                .float => |f| f.orginal.pos,
            },
            .boolean => |b| b.orginal.pos,
            .block => |b| b.end.pos,
            .identifier => |i| i.pos,
            .bin_op => |b| b.right.getRightPos(),
            .unary_op => |u| u.right.getRightPos(),
            .var_decl => |v| if (v.value) |value| value.getRightPos() else v.identifier.pos,
            .var_assign => |v| v.value.getRightPos(),
            .ifstmt => |i| i.then.getRightPos(), // TODO: Update this when add new nodes to if statement
        };
    }

    pub fn getPos(self: Node) ?DebugPos {
        const left = self.getLeftPos() orelse return null;
        const right = self.getRightPos() orelse return null;
        return left.combine(right);
    }
};

// node infos are stored in structs and in the order they come in
// so for var u8 x = 32; the first thing will come is wheter is const or not and then finally the node value;
pub const Number = union(enum) {
    integer: BaseType(i65),
    float: BaseType(f64),
};

pub const Block = struct {
    start: Token,
    nodes: []const *Node,
    end: Token,
};

pub const BinOp = struct {
    left: *Node,
    op: Token,
    right: *Node,
};

pub const UnaryOp = struct {
    op: Token,
    right: *Node,
};

pub const VariableDecl = struct {
    is_const: BaseType(bool),
    type: Token,
    identifier: Token,
    value: ?*Node,
};

pub const VariableAssign = struct {
    identifier: Token,
    value: *Node,
};

pub const IfStmt = struct {
    start: Token, // aka the if keyword token
    condition: *Node,
    then: *Node, // block or single expression
    else_node: ?*Node,

    pub fn isBlockless(self: IfStmt) bool {
        if (self.else_node) |else_node| {
            return switch (else_node.*) {
                .ifstmt => |i| i.isBlockless(),
                .block => false,
                else => true,
            };
        }
        return switch (self.then.*) {
            .ifstmt => |i| i.isBlockless(),
            .block => false,
            else => true,
        };
    }
};
