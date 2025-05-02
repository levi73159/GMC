const std = @import("std");

const Pos = @import("DebugPos.zig");
const tree = @import("tree.zig");

pub const Signal = union(enum) {
    @"break": Value,
    @"continue": void,
};

pub const Error = struct {
    msg: []const u8,
    extra: ?[]const u8,
    pos: ?Pos,
};

pub const String = struct {
    const Inner = struct {
        refs: u32,
    };
    value: []const u8,
    mem_type: union(enum) {
        heap: *Inner,
        stack: void,
    },
    allocator: std.mem.Allocator,

    pub fn deinit(self: String) void {
        std.debug.print("deinit string: {s}\n", .{self.value});
        switch (self.mem_type) {
            .heap => |h| {
                h.refs -= 1;
                std.debug.print("refs: {d}\n", .{h.refs});
                if (h.refs == 0) {
                    self.allocator.free(self.value);
                    self.allocator.destroy(h);
                }
            },
            .stack => {},
        }
    }

    pub fn clone(self: String) String {
        std.debug.print("clone string: {s}\n", .{self.value});
        return String{ .value = self.value, .mem_type = switch (self.mem_type) {
            .heap => blk: {
                const inner = self.mem_type.heap;
                inner.refs += 1;
                break :blk .{ .heap = inner };
            },
            .stack => .stack,
        }, .allocator = self.allocator };
    }

    pub fn concat(self: String, other: String) !String {
        const new_value = try std.mem.concat(self.allocator, u8, &[_][]const u8{ self.value, other.value });
        const inner = try self.allocator.create(String.Inner);
        inner.* = String.Inner{ .refs = 1 };

        self.deinit();
        other.deinit();
        return String{
            .value = new_value,
            .mem_type = .{ .heap = inner },
            .allocator = self.allocator,
        };
    }

    pub fn concatRaw(self: String, other: []const u8) !String {
        const new_value = try std.mem.concat(self.allocator, u8, &[_][]const u8{ self.value, other });
        const inner = try self.allocator.create(String.Inner);
        inner.* = String.Inner{ .refs = 1 };

        self.deinit();
        return String{
            .value = new_value,
            .mem_type = .{ .heap = inner },
            .allocator = self.allocator,
        };
    }

    /// returns error.NegativeRepeat if times < 0
    pub fn repeat(self: String, times: i65) !String {
        if (times < 0) return error.NegativeRepeat;
        if (times == 0) return String{ .value = "", .mem_type = .stack, .allocator = self.allocator };

        const new_len = self.value.len * @as(usize, @intCast(times));
        const new_value = try self.allocator.alloc(u8, new_len);
        errdefer self.allocator.free(new_value);

        var i: usize = 0;
        while (i < new_len) : (i += 1) {
            new_value[i] = self.value[i % self.value.len];
        }

        const inner = try self.allocator.create(String.Inner);
        inner.* = String.Inner{ .refs = 1 };

        self.deinit();
        return String{
            .value = new_value,
            .mem_type = .{ .heap = inner },
            .allocator = self.allocator,
        };
    }

    /// returns error.NegativeRepeat if times < 0
    pub fn initRepeat(allocator: std.mem.Allocator, char: u8, times: i65) !String {
        if (times < 0) return error.NegativeRepeat;
        if (times == 0) return String{ .value = "", .mem_type = .stack, .allocator = allocator };

        const new_len = @as(usize, @intCast(times));
        const copy_value = try allocator.alloc(u8, new_len);
        var i: usize = 0;
        while (i < new_len) : (i += 1) {
            copy_value[i] = char;
        }

        const inner = try allocator.create(String.Inner);
        inner.* = String.Inner{ .refs = 1 };

        return String{
            .value = copy_value,
            .mem_type = .{ .heap = inner },
            .allocator = allocator,
        };
    }
};

pub const Value = union(enum) {
    integer: i65,
    float: f64,
    boolean: bool,
    string: String,
    char: u8,
    none,
    runtime_error: Error,

    // converts the value to a boolean value
    pub fn convertToBool(self: Value) Value {
        return switch (self) {
            .integer => |i| Value{ .boolean = i != 0 },
            .float => |f| Value{ .boolean = f != 0.0 },
            .string => |s| Value{ .boolean = s.value.len > 0 },
            .char => |c| Value{ .boolean = c != 0 },
            .none => Value{ .boolean = false }, // none == false
            .boolean, .runtime_error => self,
        };
    }

    pub fn str(node: tree.String, force_heap: bool, allocator: std.mem.Allocator) !Value {
        if (force_heap) {
            const inner = try allocator.create(String.Inner);
            inner.* = String.Inner{ .refs = 1 };
            return Value{ .string = String{
                .value = try allocator.dupe(u8, node.n),
                .mem_type = .{ .heap = inner },
                .allocator = allocator,
            } };
        }

        if (node.allocated == .stack) {
            return Value{ .string = String{
                .value = node.n,
                .mem_type = .stack,
                .allocator = allocator,
            } };
        }

        const inner = try node.allocated.heap.create(String.Inner);
        inner.* = String.Inner{ .refs = 1 };

        return Value{ .string = String{
            .value = node.n,
            .mem_type = .{ .heap = inner },
            .allocator = allocator,
        } };
    }

    pub fn clone(self: Value) Value {
        return switch (self) {
            .string => |s| Value{ .string = s.clone() },
            else => self,
        };
    }

    pub fn deinit(self: Value) void {
        switch (self) {
            .string => |s| s.deinit(),
            else => {},
        }
    }

    pub fn err(msg: []const u8, extra: []const u8, pos: ?Pos) Value {
        return Value{ .runtime_error = Error{ .msg = msg, .extra = extra, .pos = pos } };
    }

    pub fn add(lhs: Value, rhs: Value) Value {
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
            else => Value.err("Invalid type", "Can't add nonnumeric values", null),
        };
    }

    pub fn sub(lhs: Value, rhs: Value) Value {
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

    pub fn mul(allocator: std.mem.Allocator, lhs: Value, rhs: Value) Value {
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
            .char => |c| switch (rhs) {
                .integer => |i| Value{ .string = String.initRepeat(allocator, c, i) catch |e| switch (e) {
                    error.OutOfMemory => @panic("out of memory"),
                    error.NegativeRepeat => return Value.err("Invalid Repeat", "Can't repeat by a negative number", null),
                    else => unreachable,
                } },
                else => Value.err("Invalid type", "Can't multiply nonnumeric values", null),
            },
            else => Value.err("Invalid type", "Can't multiply nonnumeric values", null),
        };
    }

    pub fn div(lhs: Value, rhs: Value) Value {
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

    pub fn mod(lhs: Value, rhs: Value) Value {
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

    pub fn bitAnd(lhs: Value, rhs: Value) Value {
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

    pub fn bitOr(lhs: Value, rhs: Value) Value {
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

    pub fn bitXor(lhs: Value, rhs: Value) Value {
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
    fn reverseShift(lhs: Value, rhs: Value, func: OpFunc) ?Value {
        if (lhs == .runtime_error) return lhs;
        if (rhs == .runtime_error) return rhs;

        if (!shiftCheck(rhs)) return Value.err("Invalid shift", "Shift value out of range", null);

        switch (rhs) {
            .integer => |i| if (i < 0) return func(lhs, Value{ .integer = -i }), // reverse shift neg to pos
            else => return null,
        }
        return null;
    }

    pub fn lshift(lhs: Value, rhs: Value) Value {
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

    pub fn rshift(lhs: Value, rhs: Value) Value {
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

    pub fn equal(lhs: Value, rhs: Value) Value {
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
                else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
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
            else => Value.err("Invalid type", "Can't compare nonnumeric values", null),
        };
    }

    pub fn notEqual(lhs: Value, rhs: Value) Value {
        return equal(lhs, rhs).not();
    }

    pub fn less(lhs: Value, rhs: Value) Value {
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

    pub fn greater(lhs: Value, rhs: Value) Value {
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

    pub fn greaterEqual(lhs: Value, rhs: Value) Value {
        const gthen = greater(lhs, rhs);
        if (gthen == .runtime_error) return gthen;
        std.debug.assert(gthen == .boolean);
        if (gthen.boolean == true) return Value{ .boolean = true };

        const eql = equal(lhs, rhs);
        if (eql == .runtime_error) return eql;
        std.debug.assert(eql == .boolean);
        return Value{ .boolean = eql.boolean };
    }

    pub fn lessEqual(lhs: Value, rhs: Value) Value {
        const lthen = less(lhs, rhs);
        if (lthen == .runtime_error) return lthen;
        std.debug.assert(lthen == .boolean);
        if (lthen.boolean == true) return Value{ .boolean = true };

        const eql = equal(lhs, rhs);
        if (eql == .runtime_error) return eql;
        std.debug.assert(eql == .boolean);
        return Value{ .boolean = eql.boolean };
    }

    pub fn neg(self: Value) Value {
        if (self == .runtime_error) return self;
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

    pub fn err(msg: []const u8, extra: []const u8, pos: ?Pos) Result {
        return Result{
            .value = Value.err(msg, extra, pos),
        };
    }

    pub fn sig(s: Signal) Result {
        return Result{
            .signal = s,
        };
    }
};
