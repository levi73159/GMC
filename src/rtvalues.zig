const std = @import("std");
const types = @import("types.zig");
const rt = @import("runtime.zig");
const castToValueNoRef = rt.castToValueNoRef;

const tree = @import("tree.zig");
const Error = types.Error;
const Pos = @import("DebugPos.zig");
const Type = @import("Type.zig");
const TypeVal = @import("Token.zig").TypeValue;
const SymbolTable = @import("SymbolTable.zig");

pub const SymbolPtr = struct {
    ptr: *SymbolTable.Symbol,
    type: Type,

    const Self = @This();

    pub fn init(ptr: *SymbolTable.Symbol, ty: TypeVal) Self {
        return Self{ .ptr = ptr, .type = ty };
    }

    pub fn deinit(self: Self) void {
        self.ptr.deinit();
    }

    pub fn ref(self: Self) Self {
        _ = self.ptr.ref();
        return self;
    }

    pub fn clone(self: Self) SymbolTable.Symbol {
        return self.ptr.clone();
    }
};

pub const Signal = union(enum) {
    @"return": Value,
    @"break": Value,
    @"continue": void,
};

pub const Value = union(enum) {
    integer: i65,
    float: f64,
    boolean: bool,
    string: types.String,
    char: u8,
    func: types.Function,
    list: *types.List,

    ptr: *Value, // all pointers are nonconst
    symbol: SymbolPtr,
    none,
    runtime_error: Error,

    // if you don't care if it's a pointer use this function to depointerize
    // depointerize 3000 will depointize a pointer if it is
    // only $9.99
    pub fn depointerizeToValue(self: Value) Value {
        return switch (self) {
            .ptr => |p| p.*,
            .symbol => |s| castToValueNoRef(s.ptr.value),
            else => self,
        };
    }

    pub fn depointerizeOnlyValue(self: Value) Value {
        return switch (self) {
            .ptr => |p| p.*,
            else => self,
        };
    }

    // converts the value to a boolean value
    pub fn convertToBool(self: Value) Value {
        defer self.deinit();
        return switch (self) {
            .integer => |i| Value{ .boolean = i != 0 },
            .float => |f| Value{ .boolean = f != 0.0 },
            .string => |s| Value{ .boolean = s.value.len > 0 },
            .char => |c| Value{ .boolean = c != 0 },
            .none => Value{ .boolean = false }, // none == false
            .func => Value{ .boolean = true }, // func == true because it have a value
            .list => |l| Value{ .boolean = l.items.len > 0 },
            .boolean, .runtime_error => self,
            .ptr => |p| p.convertToBool(),
            .symbol => |s| castToValueNoRef(s.ptr.value).convertToBool(),
        };
    }

    // converts a node (string) to a value (string)
    pub fn str(node: tree.String, force_heap: bool, allocator: std.mem.Allocator) !Value {
        if (force_heap) {
            const inner = try allocator.create(types.String.Inner);
            inner.* = types.String.Inner{ .refs = 1 };
            return Value{
                .string = types.String{
                    .value = try allocator.dupe(u8, node.n),
                    .mem_type = .{ .heap = inner },
                    .allocator = allocator,
                },
            };
        }

        if (node.allocated == .stack) {
            return Value{ .string = types.String{
                .value = node.n,
                .mem_type = .stack,
                .allocator = allocator,
            } };
        }

        const inner = try allocator.create(types.String.Inner);
        inner.* = types.String.Inner{ .refs = 1 };

        return Value{ .string = types.String{
            .value = try allocator.dupe(u8, node.n),
            .mem_type = .{ .heap = inner },
            .allocator = allocator,
        } };
    }

    /// NOTE: clones the value and returns it (will remove pointers)
    pub fn clone(self: Value) Value {
        return switch (self) {
            .string => |s| Value{ .string = s.clone() },
            .list => |l| Value{ .list = l.clone() },
            .ptr => |p| p.clone(),
            .symbol => |s| castToValueNoRef(s.clone().value),
            else => self,
        };
    }

    /// NOTE: increases the ref count (keep pointers)
    pub fn ref(self: Value) Value {
        return switch (self) {
            .string => |s| Value{ .string = s.ref() },
            .list => |l| Value{ .list = l.ref() },
            .ptr => |p| blk: {
                _ = p.ref();
                break :blk self;
            },
            .symbol => |s| Value{ .symbol = s.ref() },
            else => self,
        };
    }

    pub fn deinit(self: Value) void {
        switch (self) {
            .string => |s| s.deinit(),
            .runtime_error => |e| e.deinit(),
            .list => |l| l.deinit(),
            .ptr => |p| p.deinit(),
            else => {},
        }
    }

    pub fn err(msg: []const u8, extra: ?[]const u8, pos: ?Pos) Value {
        return Value{ .runtime_error = Error{ .msg = msg, .extra = extra, .pos = pos } };
    }

    pub fn errHeap(allocator: std.mem.Allocator, msg: []const u8, extra: []const u8, pos: ?Pos) Value {
        return Value{ .runtime_error = Error{ .msg = msg, .extra = extra, .pos = pos, .extra_allocated = allocator } };
    }

    pub fn errPrint(allocator: std.mem.Allocator, msg: []const u8, comptime fmt: []const u8, args: anytype, pos: ?Pos) Value {
        const extra = std.fmt.allocPrint(allocator, fmt, args) catch unreachable;
        return Value.errHeap(allocator, msg, extra, pos);
    }

    pub fn add(allocator: std.mem.Allocator, lhs_n: Value, rhs_n: Value) Value {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = i + j },
                .float => |f| Value{ .float = @as(f64, @floatFromInt(i)) + f },
                else => Value.err("Invalid type", "Can't add nonnumeric values", null),
            },
            .float => |f| switch (rhs) {
                .integer => |i| Value{ .float = f + @as(f64, @floatFromInt(i)) },
                .float => |j| Value{ .float = f + j },
                else => Value.err("Invalid type", "Can't add nonnumeric values", null),
            },
            .string => |s| switch (rhs) {
                .string => |t| Value{ .string = s.concat(t) catch @panic("out of memory") },
                .char => |c| Value{ .string = s.concatRaw(&[_]u8{c}) catch @panic("out of memory") },
                else => Value.err("Invalid type", "Can't add string to nonstring", null),
            },
            .char => |c| switch (rhs) {
                .string => |s| Value{ .string = types.String.fromChar(c, allocator).concat(s) catch @panic("out of memory") },
                .char => |d| Value{ .string = types.String.fromConcatChars(c, d, allocator) },
                .integer, .float => blk: {
                    const char = rt.safeIntCast(u8, rhs) catch break :blk Value.err("Invalid type", "Can't cast int or float to char (out of range 0-255)", null);
                    break :blk Value{ .string = types.String.fromConcatChars(c, char, allocator) };
                },
                else => Value.err("Invalid type", "Can't add string to nonstring", null),
            },
            .list => |l| switch (rhs) {
                .list => |r| blk: {
                    const item_type: Type = if (!l.item_type.equal(r.item_type)) Type.any() else l.item_type;
                    const new = types.List.initMutable(allocator);
                    new.item_type = item_type;

                    defer l.deinit();
                    defer r.deinit();
                    new.appendSliceSymbols(l.items) catch |e| std.debug.panic("{}", .{e});
                    new.appendSliceSymbols(r.items) catch |e| std.debug.panic("{}", .{e});
                    break :blk Value{ .list = new };
                },
                else => Value.err("Invalid type", "Can't add list to nonlist", null),
            },
            else => Value.err("Invalid type", "Can't add nonnumeric values", null),
        };
    }

    pub fn sub(lhs_n: Value, rhs_n: Value) Value {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = i - j },
                .float => |f| Value{ .float = @as(f64, @floatFromInt(i)) - f },
                else => Value.err("Invalid type", "Can't subtract nonnumeric values", null),
            },
            .float => |f| switch (rhs) {
                .integer => |i| Value{ .float = f - @as(f64, @floatFromInt(i)) },
                .float => |j| Value{ .float = f - j },
                else => Value.err("Invalid type", "Can't subtract nonnumeric values", null),
            },
            else => Value.err("Invalid type", "Can't subtract nonnumeric values", null),
        };
    }

    pub fn mul(_: std.mem.Allocator, lhs_n: Value, rhs_n: Value) Value {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = i * j },
                .float => |f| Value{ .float = @as(f64, @floatFromInt(i)) * f },
                else => Value.err("Invalid type", "Can't multiply nonnumeric values", null),
            },
            .float => |f| switch (rhs) {
                .integer => |i| Value{ .float = f * @as(f64, @floatFromInt(i)) },
                .float => |j| Value{ .float = f * j },
                else => Value.err("Invalid type", "Can't multiply nonnumeric values", null),
            },
            .string => |s| switch (rhs) {
                .integer => |i| Value{ .string = s.repeat(i) catch |e| switch (e) {
                    error.OutOfMemory => @panic("out of memory"),
                    error.NegativeRepeat => return Value.err("Invalid Repeat", "Can't repeat by a negative number", null),
                    else => unreachable,
                } },
                else => Value.err("Invalid type", "Can't multiply nonnumeric values", null),
            },
            .list => |l| switch (rhs) {
                .integer => |i| Value{ .list = l.repeat(i) catch |e| switch (e) {
                    error.OutOfMemory => @panic("out of memory"),
                    error.NegativeRepeat => return Value.err("Invalid Repeat", "Can't repeat by a negative number", null),
                    else => unreachable,
                } },
                else => Value.err("Invalid type", "Can't multiply list with noninteger", null),
            },
            else => Value.err("Invalid type", "Can't multiply nonnumeric values", null),
        };
    }

    pub fn div(lhs_n: Value, rhs_n: Value) Value {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        switch (rhs) {
            .integer => |i| if (i == 0) return Value.err("Division by zero", "Can't divide by zero", null),
            .float => |f| if (f == 0) return Value.err("Division by zero", "Can't divide by zero", null),
            else => {},
        }

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = @divTrunc(i, j) },
                .float => |f| Value{ .float = @as(f64, @floatFromInt(i)) / f },
                else => Value.err("Invalid type", "Can't divide nonnumeric values", null),
            },
            .float => |f| switch (rhs) {
                .integer => |i| Value{ .float = f / @as(f64, @floatFromInt(i)) },
                .float => |j| Value{ .float = f / j },
                else => Value.err("Invalid type", "Can't divide nonnumeric values", null),
            },
            else => Value.err("Invalid type", "Can't divide nonnumeric values", null),
        };
    }

    pub fn mod(lhs_n: Value, rhs_n: Value) Value {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = @mod(i, j) },
                .float => |f| Value{ .float = @mod(@as(f64, @floatFromInt(i)), f) },
                else => Value.err("Invalid type", "Can't mod nonnumeric values", null),
            },
            .float => |f| switch (rhs) {
                .integer => |i| Value{ .float = @mod(f, @as(f64, @floatFromInt(i))) },
                .float => |j| Value{ .float = @mod(f, j) },
                else => Value.err("Invalid type", "Can't mod nonnumeric values", null),
            },
            else => Value.err("Invalid type", "Can't mod nonnumeric values", null),
        };
    }

    pub fn bitAnd(lhs_n: Value, rhs_n: Value) Value {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = i & j },
                .float => Value.err("Invalid type", "Can't bitwise and on float", null),
                else => Value.err("Invalid type", "Can't bitwise and nonnumeric values", null),
            },
            .float => Value.err("Invalid type", "Can't bitwise and on float", null),
            else => Value.err("Invalid type", "Can't bitwise and nonnumeric values", null),
        };
    }

    pub fn bitOr(lhs_n: Value, rhs_n: Value) Value {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = i | j },
                .float => Value.err("Invalid type", "Can't bitwise or on float", null),
                else => Value.err("Invalid type", "Can't bitwise or nonnumeric values", null),
            },
            .float => Value.err("Invalid type", "Can't bitwise or on float", null),
            else => Value.err("Invalid type", "Can't bitwise or nonnumeric values", null),
        };
    }

    pub fn bitXor(lhs_n: Value, rhs_n: Value) Value {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = i ^ j },
                .float => Value.err("Invalid type", "Can't bitwise xor on float", null),
                else => Value.err("Invalid type", "Can't bitwise xor nonnumeric values", null),
            },
            .float => Value.err("Invalid type", "Can't bitwise xor on float", null),
            else => Value.err("Invalid type", "Can't bitwise xor nonnumeric values", null),
        };
    }

    fn shiftCheck(rhs: Value) bool {
        const check2 = switch (rhs) {
            .integer => |i| i < @bitSizeOf(i65),
            else => true, // so we can handle checking that ourself
        };

        return check2;
    }

    const OpFunc = fn (lhs: Value, rhs: Value) Value;
    fn reverseShift(lhs_n: Value, rhs_n: Value, func: OpFunc) ?Value {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        if (!shiftCheck(rhs)) return Value.err("Invalid shift", "Shift value out of range", null);

        switch (rhs) {
            .integer => |i| if (i < 0) return func(lhs, Value{ .integer = -i }), // reverse shift neg to pos
            else => return null,
        }
        return null;
    }

    pub fn lshift(lhs_n: Value, rhs_n: Value) Value {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        if (reverseShift(lhs, rhs, rshift)) |v| return v;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = i << @truncate(@as(u65, @intCast(j))) },
                .float => Value.err("Invalid type", "Can't lshift on float", null),
                else => Value.err("Invalid type", "Can't lshift nonnumeric values", null),
            },
            .float => Value.err("Invalid type", "Can't lshift on float", null),
            else => Value.err("Invalid type", "Can't lshift nonnumeric values", null),
        };
    }

    pub fn rshift(lhs_n: Value, rhs_n: Value) Value {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        if (reverseShift(lhs, rhs, lshift)) |v| return v;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .integer = i >> @truncate(@as(u65, @intCast(j))) },
                .float => Value.err("Invalid type", "Can't rshift on float", null),
                else => Value.err("Invalid type", "Can't rshift nonnumeric values", null),
            },
            .float => Value.err("Invalid type", "Can't rshift on float", null),
            else => Value.err("Invalid type", "Can't rshift nonnumeric values", null),
        };
    }

    // logical operators
    pub fn logAnd(lhs: Value, rhs: Value) Value {
        const lhsb = lhs.convertToBool(); // convert to bool or runtime_error
        if (lhsb == .runtime_error) return lhsb;
        if (!lhsb.boolean) return Value{ .boolean = false };

        return rhs.convertToBool();
    }

    pub fn logOr(lhs: Value, rhs: Value) Value {
        const lhsb = lhs.convertToBool(); // convert to bool or runtime_error
        if (lhsb == .runtime_error) return lhsb;
        if (lhsb.boolean) return Value{ .boolean = true };

        return rhs.convertToBool();
    }

    // this equal function is a bit different
    // it returns a bool instead of a Value use for comparisions outside of the runtime
    pub fn equalB(lhs_n: Value, rhs_n: Value) bool {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        if (lhs == .runtime_error) return false;
        if (rhs == .runtime_error) return false;

        switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| return i == j,
                .float => |f| return @as(f64, @floatFromInt(i)) == f,
                .boolean => |b| return b == (i != 0),
                .none => return i == 0,
                else => return false,
            },
            .float => |f| switch (rhs) {
                .integer => |i| return f == @as(f64, @floatFromInt(i)),
                .float => |j| return f == j,
                .boolean => |b| return b == (f != 0.0),
                .none => return f == 0.0,
                else => return false,
            },
            .boolean => |b| switch (rhs) {
                .integer => |i| return b == (i != 0),
                .float => |f| return b == (f != 0.0),
                .boolean => |j| return b == j,
                .none => return !b,
                else => return false,
            },
            .none => switch (rhs) {
                .integer => |i| return i == 0,
                .float => |f| return f == 0.0,
                .boolean => |b| return !b,
                .none => return true,
                else => return false,
            },
            .string => |s| switch (rhs) {
                .string => |j| return s.equal(j),
                .char => |c| return s.equalChar(c),
                .none => return s.value.len == 0,
                else => return false,
            },
            .char => |c| switch (rhs) {
                .char => |j| return c == j,
                .string => |s| return s.equalChar(c),
                .none => return c == 0,
                else => return false,
            },
            .list => |l| switch (rhs) {
                .list => |j| return l.equal(j),
                .none => return l.items.len == 0,
                else => return false,
            },
            else => return false,
        }
    }

    pub fn equal(lhs_n: Value, rhs_n: Value) Value {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .boolean = i == j },
                .float => |f| Value{ .boolean = @as(f64, @floatFromInt(i)) == f },
                .boolean => |b| Value{ .boolean = b == (i != 0) },
                .none => Value{ .boolean = i == 0 },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .float => |f| switch (rhs) {
                .integer => |i| Value{ .boolean = f == @as(f64, @floatFromInt(i)) },
                .float => |j| Value{ .boolean = f == j },
                .boolean => |b| Value{ .boolean = b == (f != 0.0) },
                .none => Value{ .boolean = f == 0.0 },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .boolean => |b| switch (rhs) {
                .integer => |i| Value{ .boolean = b == (i != 0) },
                .float => |f| Value{ .boolean = b == (f != 0.0) },
                .boolean => |j| Value{ .boolean = b == j },
                .char => |c| Value{ .boolean = b == (c != 0) },
                .string => |s| Value{ .boolean = b == (s.value.len > 0) },
                .none => Value{ .boolean = !b },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .string => |s| switch (rhs) {
                .string => |j| Value{ .boolean = s.value.len == j.value.len and std.mem.eql(u8, s.value, j.value) },
                .char => |c| Value{ .boolean = s.value.len == 1 and s.value[0] == c },
                .none => Value{ .boolean = s.value.len == 0 },
                else => Value.err("Invalid type", "Can't compare strings to nonstring values", null),
            },
            .char => |c| switch (rhs) {
                .char => |j| Value{ .boolean = c == j },
                .string => |s| Value{ .boolean = s.value.len == 1 and s.value[0] == c },
                .none => Value{ .boolean = c == 0 },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .none => switch (rhs) {
                .integer => |i| Value{ .boolean = i == 0 },
                .float => |f| Value{ .boolean = f == 0.0 },
                .boolean => |b| Value{ .boolean = false == b },
                .string => |s| Value{ .boolean = s.value.len == 0 },
                .char => |c| Value{ .boolean = c == 0 },
                .none => Value{ .boolean = true },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .func => Value.err("Invalid type", "Can't compare functions", null), // you can techincally compare functions but that's a bad idea hince the error
            .list => |l| switch (rhs) {
                .list => |j| l.equal(j),
                .none => Value{ .boolean = l.items.len == 0 },
                else => Value.err("Invalid type", "Can't compare list to nonlist values", null),
            },
            else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
        };
    }

    pub fn notEqual(lhs: Value, rhs: Value) Value {
        return equal(lhs, rhs).not();
    }

    pub fn less(lhs_n: Value, rhs_n: Value) Value {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .boolean = i < j },
                .float => |f| Value{ .boolean = @as(f64, @floatFromInt(i)) < f },
                .none => Value{ .boolean = i < 0 },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .float => |f| switch (rhs) {
                .integer => |i| Value{ .boolean = f < @as(f64, @floatFromInt(i)) },
                .float => |j| Value{ .boolean = f < j },
                .none => Value{ .boolean = f < 0.0 },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .none => switch (rhs) {
                .integer => |i| Value{ .boolean = 0 < i },
                .float => |f| Value{ .boolean = 0.0 < f },
                .none => Value{ .boolean = false },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
        };
    }

    pub fn greater(lhs_n: Value, rhs_n: Value) Value {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        return switch (lhs) {
            .integer => |i| switch (rhs) {
                .integer => |j| Value{ .boolean = i > j },
                .float => |f| Value{ .boolean = @as(f64, @floatFromInt(i)) > f },
                .none => Value{ .boolean = i > 0 },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .float => |f| switch (rhs) {
                .integer => |i| Value{ .boolean = f > @as(f64, @floatFromInt(i)) },
                .float => |j| Value{ .boolean = f > j },
                .none => Value{ .boolean = f > 0.0 },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            .none => switch (rhs) {
                .integer => |i| Value{ .boolean = 0 > i },
                .float => |f| Value{ .boolean = 0.0 > f },
                .none => Value{ .boolean = false },
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
            },
            else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
        };
    }

    pub fn greaterEqual(lhs_n: Value, rhs_n: Value) Value {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        const gthen = greater(lhs, rhs);

        if (gthen == .runtime_error) return gthen;
        std.debug.assert(gthen == .boolean);
        if (gthen.boolean == true) return Value{ .boolean = true };

        const eql = equal(lhs, rhs);
        if (eql == .runtime_error) return eql;
        std.debug.assert(eql == .boolean);
        return Value{ .boolean = eql.boolean };
    }

    pub fn lessEqual(lhs_n: Value, rhs_n: Value) Value {
        const lhs = lhs_n.depointerizeToValue();
        const rhs = rhs_n.depointerizeToValue();

        const lthen = less(lhs, rhs);

        if (lthen == .runtime_error) return lthen;
        std.debug.assert(lthen == .boolean);
        if (lthen.boolean == true) return Value{ .boolean = true };

        const eql = equal(lhs, rhs);
        if (eql == .runtime_error) return eql;
        std.debug.assert(eql == .boolean);
        return Value{ .boolean = eql.boolean };
    }

    pub fn neg(self_n: Value) Value {
        if (self_n == .runtime_error) return self_n;

        const self = if (self_n == .ptr) self_n.ptr.* else self_n;

        return switch (self) {
            .integer => |i| Value{ .integer = -i },
            .float => |f| Value{ .float = -f },
            else => Value.err("Invalid type", "Can't negate nonnumeric values", null),
        };
    }

    pub fn not(self: Value) Value {
        const val = self.convertToBool(); // convert to bool or runtime_error
        if (val == .runtime_error) return val;
        std.debug.assert(val == .boolean);
        return Value{ .boolean = !val.boolean };
    }

    pub fn index(self_n: Value, i_n: Value) Value {
        const self = self_n.depointerizeToValue();
        const i = i_n.depointerizeToValue();

        if (self == .runtime_error) return self;
        if (i == .runtime_error) return i;
        return switch (self) {
            .string => |s| s.index(i),
            .list => |l| l.index(i),
            else => Value.err("Invalid type", "Value is not indexable", null),
        };
    }

    pub fn field(self_n: Value, name: []const u8) Value {
        const self = self_n.depointerizeToValue();
        if (self == .runtime_error) return self;

        return switch (self) {
            .list => |l| l.field(name),
            else => Value.err("Invalid Type", "Value doesn't have any fields (not a struct)", null),
        };
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        // std.debug.print("Value: {s}\n", .{@tagName(self)});
        switch (self) {
            .integer => |i| try writer.print("{}", .{i}),
            .float => |f| try writer.print("{d}", .{f}),
            .boolean => |b| try writer.print("{}", .{b}),
            .char => |c| try writer.print("{c}", .{c}),
            .none => try writer.print("void", .{}),
            .runtime_error => |e| {
                try writer.print("Runtime error: {s}\n", .{e.msg});
                if (e.extra) |extra| {
                    try writer.print("{s}\n", .{extra});
                }
                if (e.pos) |pos| {
                    try writer.print("{}\n", .{pos});
                    try writer.print("line: {d}, column: {d}\n", .{ pos.line, pos.column });
                }
            },
            .string => |s| try writer.print("{s}", .{s.value}),
            .func => |f| try writer.print("[func {s}]", .{f.getName()}),
            .list => |l| {
                try writer.writeByte('[');
                for (l.items, 0..) |item, i| {
                    if (i != 0) try writer.writeByte(' ');
                    if (item.value == .string) try writer.writeByte('"');
                    if (item.value == .char) try writer.writeByte('\'');
                    try writer.print("{}", .{castToValueNoRef(item.value)});
                    if (item.value == .char) try writer.writeByte('\'');
                    if (item.value == .string) try writer.writeByte('"');
                    if (i != l.items.len - 1) try writer.writeByte(',');
                }
                try writer.writeByte(']');
            },
            .ptr, .symbol => try writer.print("{}", .{self.depointerizeToValue()}),
        }
    }
};

pub const Result = union(enum) {
    value: Value,
    signal: Signal,

    pub fn none() Result {
        return Result{
            .value = Value{ .none = {} },
        };
    }

    pub fn val(v: Value) Result {
        return Result{
            .value = v,
        };
    }

    pub fn err(msg: []const u8, extra: ?[]const u8, pos: ?Pos) Result {
        return Result{
            .value = Value.err(msg, extra, pos),
        };
    }

    pub fn errHeap(allocator: std.mem.Allocator, msg: []const u8, extra: []const u8, pos: ?Pos) Result {
        var rterr = Value.err(msg, extra, pos);
        rterr.runtime_error.extra_allocated = allocator;
        return Result{
            .value = rterr,
        };
    }

    pub fn errPrint(allocator: std.mem.Allocator, msg: []const u8, comptime fmt: []const u8, args: anytype, pos: ?Pos) Result {
        const extra = std.fmt.allocPrint(allocator, fmt, args) catch unreachable;
        return Result.errHeap(allocator, msg, extra, pos);
    }

    pub fn sig(s: Signal) Result {
        return Result{
            .signal = s,
        };
    }

    pub fn deinit(self: Result) void {
        switch (self) {
            .value => |v| v.deinit(),
            else => {},
        }
    }
};
