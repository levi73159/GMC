const std = @import("std");
const Pos = @import("DebugPos.zig");
const tree = @import("tree.zig");

const Intrepreter = @import("Interpreter.zig");
const Params = tree.FuncParam;
const SymbolTable = @import("SymbolTable.zig");

const rt = @import("runtime.zig");
const Value = rt.Value;
const Result = rt.Result;

const Type = @import("Type.zig");
pub const TypeVal = @import("Token.zig").TypeValue;

pub const List = @import("types/List.zig");
pub const String = @import("types/String.zig");
pub const Enum = @import("types/Enum.zig");
pub const TypeInfo = @import("types/TypeInfo.zig");

pub const Error = struct {
    msg: []const u8,
    extra: ?[]const u8,
    pos: ?Pos,

    extra_allocated: ?std.mem.Allocator = null, // null = false (default aka stack), value = allocated
    pub fn deinit(self: Error) void {
        if (self.extra_allocated) |allocator| {
            if (self.extra) |extra| allocator.free(extra);
        }
    }

    pub fn equal(_: Error, _: Error) bool {
        return false;
    }
};

pub const BaseFunction = struct {
    name: []const u8,
    params: []const tree.FuncParam,
    body: *const tree.Node, // root node to execute
    return_type: Type,
    parent_scope: ?*SymbolTable,

    // debug pos, might be usefull but not needed
    name_pos: ?Pos = null,
    return_type_pos: ?Pos = null,

    pub fn call(self: BaseFunction, args: []const Value, base: Intrepreter) Result {
        var symbols = SymbolTable.init(base.allocator);
        defer symbols.deinit();

        if (self.params.len != args.len) {
            const msg = std.fmt.allocPrint(base.allocator, "Expected {d} arguments got {d}", .{ self.params.len, args.len }) catch unreachable;
            return Result.errHeap(base.allocator, "Invalid number of arguments", msg, null);
        }

        for (self.params, args) |param, arg| {
            var symbol_value: SymbolTable.SymbolValue = undefined;
            const rs = rt.castToTypeWithErrorMessage(base.allocator, arg, param.type, &symbol_value);
            if (rt.checkRuntimeErrorOrSignal(rs, self.body)) |err| return err;

            symbols.add(param.name.lexeme, SymbolTable.Symbol{ .value = symbol_value, .is_const = true }) catch @panic("out of memory");
        }
        // setting scope heres mean that if there is a param x and a varaible x outside of the function it will be shadowed by the Params
        // if we don't wan't that and want it to cause an error we can put this line before we add the params
        symbols.parent = self.parent_scope;

        const scope = base.newScope(&symbols, base.allocator);
        const ret = scope.evalNode(self.body);
        if (rt.checkRuntimeErrorOrSignal(ret, self.body)) |sigOrErr| return sigOrErr;
        return rt.Result.none();
    }

    // not the best but get the job done
    pub fn staticCheck(self: BaseFunction, base: Intrepreter) ?Result {
        var arena = std.heap.ArenaAllocator.init(base.allocator);
        defer arena.deinit();

        const allocator = arena.allocator();

        var symbols = SymbolTable.init(allocator);
        defer symbols.deinit();

        for (self.params) |param| {
            var symbol_value: SymbolTable.SymbolValue = undefined;
            const rs = rt.castToTypeWithErrorMessage(base.allocator, .none, param.type, &symbol_value);
            if (rt.checkRuntimeErrorOrSignal(rs, self.body)) |err| return err;

            symbols.add(param.name.lexeme, SymbolTable.Symbol{ .value = symbol_value, .is_const = true }) catch @panic("out of memory");
        }

        symbols.parent = self.parent_scope;

        var scope = base.newScope(&symbols, allocator);
        scope.static = true;
        const ret = scope.evalNode(self.body);
        if (rt.checkRuntimeErrorOrSignal(ret, self.body)) |sigOrErr| switch (sigOrErr) {
            .signal => |signal| switch (signal) {
                .@"return" => |v| {
                    _ = rt.castToType(allocator, v, self.return_type) catch {
                        const msg = std.fmt.allocPrint(
                            base.allocator,
                            "Function return an expected type, Expected {s} got {s}",
                            .{ @tagName(self.return_type.value), @tagName(ret.value) },
                        ) catch unreachable;
                        return Result.errHeap(base.allocator, "Invalid return type", msg, self.return_type_pos);
                    };
                    // we can cast the value
                    return null;
                },
                else => return sigOrErr,
            },
            else => return sigOrErr,
        };

        if (self.return_type.value != .void) {
            const msg = std.fmt.allocPrint(base.allocator, "Function return an expected type, Expected {s} got void", .{@tagName(self.return_type.value)}) catch unreachable;
            return Result.errHeap(base.allocator, "Invalid return type", msg, self.return_type_pos);
        }

        return null;
    }
};

pub const BultinFunction = struct {
    name: []const u8,
    func: *const fn (args: []const Value, base: Intrepreter) Result,

    is_method: bool = false,

    pub fn call(self: BultinFunction, args: []const Value, base: Intrepreter) Result {
        return self.func(args, base);
    }

    pub fn callWithType(self: BultinFunction, this: Value, args: []const Value, base: Intrepreter) Result {
        const new_args = std.mem.concat(base.allocator, Value, &[_][]const Value{ &.{this}, args }) catch unreachable;
        defer base.allocator.free(new_args);

        return self.func(new_args, base);
    }
};

pub const Function = union(enum) {
    base: BaseFunction,
    bultin: BultinFunction,

    pub fn getName(self: Function) []const u8 {
        return switch (self) {
            .base => |f| f.name,
            .bultin => |f| f.name,
        };
    }

    pub fn isMethod(self: Function) bool {
        return switch (self) {
            .base => false,
            .bultin => |f| f.is_method,
        };
    }

    pub fn call(self: Function, args: []const Value, base: Intrepreter) Result {
        return switch (self) {
            .base => |f| f.call(args, base),
            .bultin => |f| f.call(args, base),
        };
    }

    pub fn callWithType(self: Function, this: Value, args: []const Value, base: Intrepreter) Result {
        return switch (self) {
            .base => Result.err("Not supported", "Feature not supported", null),
            .bultin => |f| f.callWithType(this, args, base),
        };
    }

    pub fn equal(self: Function, other: Function) bool {
        const self_name = self.getName();
        const other_name = other.getName();
        return std.mem.eql(u8, self_name, other_name);
    }
};
