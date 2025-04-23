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
    identifier: Token,
    bin_op: BinOp,
    unary_op: UnaryOp,
    var_decl: VariableDecl,
    var_assign: VariableAssign,

    pub fn getLeftPos(self: Node) ?DebugPos {
        return switch (self) {
            .number => |n| switch (n) {
                .integer => |i| i.orginal.pos,
                .float => |f| f.orginal.pos,
            },
            .identifier => |i| i.pos,
            .bin_op => |b| b.left.getLeftPos(),
            .unary_op => |u| u.op.pos,
            .var_decl => |v| v.is_const.orginal.pos,
            .var_assign => |v| v.identifier.pos,
        };
    }

    pub fn getRightPos(self: Node) ?DebugPos {
        return switch (self) {
            .number => |n| switch (n) {
                .integer => |i| i.orginal.pos,
                .float => |f| f.orginal.pos,
            },
            .identifier => |i| i.pos,
            .bin_op => |b| b.right.getRightPos(),
            .unary_op => |u| u.right.getRightPos(),
            .var_decl => |v| if (v.value) |value| value.getRightPos() else v.identifier.pos,
            .var_assign => |v| v.value.getRightPos(),
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
