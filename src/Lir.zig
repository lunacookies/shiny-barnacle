const std = @import("std");
const indexer = @import("indexer.zig");
const utils = @import("utils.zig");
const Ast = @import("Ast.zig");
const Allocator = std.mem.Allocator;
const TextRange = @import("TextRange.zig");
const Lir = @This();

bodies: std.StringHashMap(Body),

pub const Body = struct {
    instructions: std.ArrayList(Instruction),
    local_types: std.ArrayList(Type),
};

// At some point this may need to change, when types get more complex
// The IRType should only really care about things like sizes, so should be an enum
pub const IRType = Type;

pub const Instruction = struct {
    data: Data,
    /// The type associated with an instruction, in most cases this is the return type
    /// If the instruction doesn't have an associated type then this value is undefined
    ty: IRType,
    range: TextRange,

    pub const Data = union(enum) {
        push: u64,
        lst: u32,
        lld: u32,
        ret,
        add,
        sub,
        mul,
        div,
        mod,
        or_,
        and_,
        xor,
        shl,
        shr,
        lt,
        le,
        gt,
        ge,
        eq,
        ne,
        /// Cast from this type to the return type given by the IRType
        cast: Type,
    };
};

pub const Type = union(enum) {
    i32,
    i64,
    u32,
    u64,
    u8,

    pub fn size(self: Type) u32 {
        switch (self) {
            .i32, .u32 => return 4,
            .i64, .u64 => return 8,
            .u8 => return 1,
        }
    }

    pub fn alignment(self: Type) u32 {
        return self.size();
    }

    pub fn isSigned(self: Type) bool {
        switch (self) {
            .i32, .i64 => return true,
            .u32, .u64, .u8 => return false,
        }
    }

    pub fn isNumeric(self: Type) bool {
        return switch (self) {
            .u32, .u64, .i32, .i64, .u8 => true,
        };
    }

    pub fn isEqual(a: Type, b: Type) bool {
        return std.meta.eql(a, b);
    }

    pub fn toZigType(comptime a: Type) type {
        return switch (a) {
            .u8 => u8,
            .u32 => u32,
            .u64 => u64,
            .i32 => i32,
            .i64 => i64,
        };
    }

    /// Returns whether it is possible to implicitly cast an `a` to a `b`
    pub fn isSubtype(a: Type, b: Type) bool {
        if (isEqual(a, b)) return true;
        if (!(a.isNumeric() and b.isNumeric())) return false;
        return a.size() < b.size() and (b.isSigned() or !a.isSigned());
    }

    // Only really makes sense if you want to know if you know a and b aren't
    // equal, but want to find out if you can still cast
    pub fn isStrictSubtype(a: Type, b: Type) bool {
        if (!(a.isNumeric() and b.isNumeric())) return false;
        return a.size() < b.size() and (b.isSigned() or !a.isSigned());
    }

    fn format(
        self: Type,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .i32 => try writer.writeAll("i32"),
        }
    }
};

pub fn analyze(
    input: []const u8,
    file_index: indexer.FileIndex,
    ast: Ast,
    a: Allocator,
) !Lir {
    var analyzer = Analyzer{
        .lir = .{ .bodies = std.StringHashMap(Body).init(a) },
        .file_index = file_index,
        .input = input,
    };

    try analyzer.analyze(ast, a);
    return analyzer.lir;
}

const Analyzer = struct {
    lir: Lir,
    file_index: indexer.FileIndex,
    input: []const u8,

    fn analyze(self: *Analyzer, ast: Ast, a: Allocator) !void {
        for (ast.items.items) |ast_item| {
            try switch (ast_item.data) {
                .function => |function| self.analyzeFunction(ast_item.name, function, a),
            };
        }
    }

    fn analyzeFunction(self: *Analyzer, name: []const u8, function: Ast.Item.Function, a: Allocator) !void {
        // The indexer has already handled raising an error about this.
        std.debug.assert(!self.lir.bodies.contains(name));

        var function_analyzer = FunctionAnalyzer.init(a, self.file_index, self.input);
        try function_analyzer.analyzeFunction(function);

        try self.lir.bodies.put(name, function_analyzer.body);
    }
};

const FunctionAnalyzer = struct {
    body: Body,
    scopes: std.ArrayList(std.StringHashMapUnmanaged(ScopeEntry)),
    file_index: indexer.FileIndex,
    input: []const u8,
    allocator: Allocator,

    const ScopeEntry = struct {
        ty: Type,
        index: u32,
    };

    fn init(a: Allocator, file_index: indexer.FileIndex, input: []const u8) @This() {
        return FunctionAnalyzer{
            .body = .{
                .instructions = std.ArrayList(Instruction).init(a),
                .local_types = std.ArrayList(Type).init(a),
            },
            .scopes = std.ArrayList(std.StringHashMapUnmanaged(ScopeEntry)).init(a),
            .file_index = file_index,
            .input = input,
            .allocator = a,
        };
    }

    fn analyzeFunction(
        self: *FunctionAnalyzer,
        function: Ast.Item.Function,
    ) !void {
        try self.pushScope();
        try self.analyzeStatement(function.body);
        self.popScope();
    }

    fn analyzeStatement(self: *FunctionAnalyzer, statement: Ast.Statement) !void {
        switch (statement.data) {
            .local_declaration => |ld| {
                const expected_type = self.analyzeType(ld.ty);
                const actual_type = try self.analyzeExpression(ld.value, expected_type);
                // self.ensureTypesMatch(expected_type, actual_type, statement.range);
                std.debug.assert(actual_type.isEqual(expected_type));

                const local_index = try self.createLocal(ld.name, actual_type);
                const i = .{ .lst = local_index };
                try self.pushInstruction(i, expected_type, statement.range);
            },

            .return_ => |return_| {
                _ = try self.analyzeExpression(return_.value, .i32);
                try self.pushInstruction(.ret, .i32, statement.range);
            },

            .block => |block| {
                try self.pushScope();
                for (block.statements.items) |s| {
                    try self.analyzeStatement(s);
                }
                self.popScope();
            },
        }
    }

    fn parseInt(integer_str: []const u8, ty: Type) !u64 {
        return switch (ty) {
            .u8 => @intCast(u64, try std.fmt.parseInt(u8, integer_str, 10)),
            .u32 => @intCast(u64, try std.fmt.parseInt(u32, integer_str, 10)),
            .u64 => @intCast(u64, try std.fmt.parseInt(u64, integer_str, 10)),
            .i32 => @bitCast(u64, @intCast(i64, try std.fmt.parseInt(i32, integer_str, 10))),
            .i64 => @bitCast(u64, try std.fmt.parseInt(i64, integer_str, 10)),
        };
    }

    fn analyzeExpression(self: *FunctionAnalyzer, expression: Ast.Expression, expected_type_opt: ?Type) !Type {
        switch (expression.data) {
            .integer => |integer_str| {
                if (expected_type_opt) |expected_type| {
                    // There's probably a nicer way to do this, but I couldn't find how to get it to unroll the union(enum)
                    const integer = parseInt(integer_str, expected_type) catch
                        self.emitError(expression.range, "integer “{s}” out of range for type “{}”", .{ integer_str, expected_type });
                    const i = .{ .push = integer };
                    try self.pushInstruction(i, expected_type, expression.range);
                    return expected_type;
                } else {
                    self.emitError(expression.range, "cannot infer type of integer", .{});
                }
            },

            .name => |name| {
                const scopeEntry = self.lookupLocal(name, expression.range);
                try self.pushInstruction(.{ .lld = scopeEntry.index }, scopeEntry.ty, expression.range);
                const castedTy = try self.checkAndCast(scopeEntry.ty, expected_type_opt, expression.range);
                return castedTy;
            },

            .binary => |binary| {
                const lhs = binary.lhs.*;
                const rhs = binary.rhs.*;

                const lhs_type = try self.analyzeExpression(lhs, expected_type_opt);
                self.ensureTypeNumeric(lhs_type, lhs.range);
                const rhs_type = try self.analyzeExpression(rhs, lhs_type);
                std.debug.assert(lhs_type.isEqual(rhs_type));
                // self.ensureTypeNumeric(rhs_type, rhs.range);
                // self.ensureTypesMatch(lhs_type, rhs_type, rhs.range);
                const ty = lhs_type;

                const instruction: Instruction.Data = switch (binary.op) {
                    .add => .add,
                    .subtract => .sub,
                    .multiply => .mul,
                    .divide => .div,
                    .modulus => .mod,
                    .bitwise_or => .or_,
                    .bitwise_and => .and_,
                    .xor => .xor,
                    .shift_left => .shl,
                    .shift_right => .shr,
                    .less_than => .lt,
                    .less_than_equal => .le,
                    .greater_than => .gt,
                    .greater_than_equal => .ge,
                    .equal => .eq,
                    .not_equal => .ne,
                };
                try self.pushInstruction(instruction, ty, expression.range);

                return ty;
            },
        }
    }

    fn analyzeType(self: *FunctionAnalyzer, ty: Ast.Type) Type {
        if (std.mem.eql(u8, ty.name, "i32")) return .i32;
        if (std.mem.eql(u8, ty.name, "i64")) return .i64;
        if (std.mem.eql(u8, ty.name, "u32")) return .u32;
        if (std.mem.eql(u8, ty.name, "u64")) return .u64;
        if (std.mem.eql(u8, ty.name, "u8")) return .u8;
        self.emitError(ty.range, "unknown type", .{});
    }

    fn createLocal(
        self: *FunctionAnalyzer,
        name: []const u8,
        ty: Type,
    ) !u32 {
        const last_scope = &self.scopes.items[self.scopes.items.len - 1];
        const i = @intCast(u32, self.body.local_types.items.len);
        try last_scope.put(self.allocator, name, ScopeEntry{ .ty = ty, .index = i });
        try self.body.local_types.append(ty);
        return i;
    }

    fn lookupLocal(self: *@This(), name: []const u8, range: TextRange) *ScopeEntry {
        var i = self.scopes.items.len - 1;
        while (i >= 0) : (i += 1) {
            if (self.scopes.items[i].getPtr(name)) |entry| {
                return entry;
            }
        }
        self.emitError(range, "undeclared variable “{}”\n", .{name});
    }

    fn ensureTypeNumeric(self: *@This(), ty: Type, range: TextRange) void {
        if (ty.isNumeric()) return;

        self.emitError(
            range,
            "expected numeric type but found “{}”\n",
            .{ty},
        );
    }

    fn ensureTypesMatch(
        self: *FunctionAnalyzer,
        expected: Type,
        actual: Type,
        range: TextRange,
    ) void {
        if (std.meta.eql(expected, actual)) return;

        self.emitError(
            range,
            "expected type “{}” but found “{}”\n",
            .{ expected, actual },
        );
    }

    fn emitError(
        self: *const FunctionAnalyzer,
        range: TextRange,
        comptime fmt: []const u8,
        args: anytype,
    ) noreturn {
        const line_col = utils.indexToLineCol(self.input, range.start);

        std.debug.print("{}: error: ", .{line_col});
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});
        std.os.exit(92);
    }

    fn pushUntypedInstruction(
        self: *FunctionAnalyzer,
        data: Instruction.Data,
        range: TextRange,
    ) !void {
        const instruction = .{
            .data = data,
            .ty = undefined,
            .range = range,
        };
        try self.body.instructions.append(instruction);
    }

    fn pushInstruction(
        self: *FunctionAnalyzer,
        data: Instruction.Data,
        ty: IRType,
        range: TextRange,
    ) !void {
        const instruction = .{
            .data = data,
            .ty = ty,
            .range = range,
        };
        try self.body.instructions.append(instruction);
    }

    fn checkAndCast(self: *@This(), src: Type, dest: ?Type, range: TextRange) !Type {
        if (dest) |d| {
            if (src.isEqual(d)) return d;
            if (src.isSubtype(d)) {
                try self.body.instructions.append(.{
                    .data = .{ .cast = src },
                    .ty = d,
                    .range = range,
                });
                return d;
            }
            self.emitError(range, "expected type “{}” but found “{}”\n", .{ d, src });
        }
        return src;
    }

    fn pushScope(self: *FunctionAnalyzer) !void {
        const new_scope = std.StringHashMapUnmanaged(ScopeEntry){};
        try self.scopes.append(new_scope);
    }

    fn popScope(self: *FunctionAnalyzer) void {
        _ = self.scopes.pop();
    }
};

pub fn format(
    self: Lir,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;

    var bounded_array = std.BoundedArray(u8, pretty_print_buf_size).init(0) catch unreachable;

    var ctx = PrettyPrintContext{ .indentation = 0, .writer = bounded_array.writer() };
    ctx.printLir(self) catch unreachable;
    try writer.writeAll(bounded_array.slice());
}

const pretty_print_buf_size = 1024 * 1024;

const PrettyPrintContext = struct {
    indentation: usize,
    writer: std.BoundedArray(u8, pretty_print_buf_size).Writer,

    const Error = error{Overflow};

    fn printLir(self: *PrettyPrintContext, lir: Lir) Error!void {
        var iterator = lir.bodies.iterator();
        var i: u32 = 0;
        while (iterator.next()) |entry| : (i += 1) {
            const name = entry.key_ptr.*;
            const body = entry.value_ptr.*;

            if (i != 0) try self.writer.writeAll("\n\n");
            try self.printBody(name, body);
        }
    }

    fn printBody(
        self: *PrettyPrintContext,
        name: []const u8,
        body: Body,
    ) Error!void {
        try self.writer.print("func {s} {{", .{name});

        if (body.instructions.items.len == 0) {
            try self.writer.writeByte('}');
            return;
        }

        for (body.instructions.items) |instruction| {
            try self.writer.writeAll("\n\t");
            try self.printInstruction(instruction);
        }

        try self.writer.writeAll("\n}");
    }

    fn printInstruction(
        self: *PrettyPrintContext,
        instruction: Instruction,
    ) Error!void {
        try self.writer.print("<{}>\t", .{instruction.ty});
        switch (instruction.data) {
            .push => |integer| try self.writer.print("push\t{}", .{integer}),
            .lst => |local_index| try self.writer.print("lst\t{}", .{local_index}),
            .lld => |local_index| try self.writer.print("lld\t{}", .{local_index}),
            .ret => try self.writer.writeAll("ret"),
            .add => try self.writer.writeAll("add"),
            .sub => try self.writer.writeAll("sub"),
            .mul => try self.writer.writeAll("mul"),
            .div => try self.writer.writeAll("div"),
            .mod => try self.writer.writeAll("mod"),
            .or_ => try self.writer.writeAll("or"),
            .and_ => try self.writer.writeAll("and"),
            .xor => try self.writer.writeAll("xor"),
            .shl => try self.writer.writeAll("shl"),
            .shr => try self.writer.writeAll("shr"),
            .lt => try self.writer.writeAll("lt"),
            .le => try self.writer.writeAll("le"),
            .gt => try self.writer.writeAll("gt"),
            .ge => try self.writer.writeAll("ge"),
            .eq => try self.writer.writeAll("eq"),
            .ne => try self.writer.writeAll("ne"),
            .cast => |ty| try self.writer.print("cast\t{}", .{ty}),
        }
    }

    fn printFunction(
        self: *PrettyPrintContext,
        name: []const u8,
        function: Ast.Item.Function,
    ) Error!void {
        try self.writer.print("func {s} ", .{name});
        try self.printStatement(function.body);
    }

    fn printType(self: *PrettyPrintContext, ty: Ast.Type) Error!void {
        try self.writer.writeAll(ty.name);
    }

    fn printStatement(self: *PrettyPrintContext, statement: Ast.Statement) Error!void {
        try switch (statement.data) {
            .local_declaration => |ld| self.printLocalDeclaration(ld),
            .block => |block| self.printBlock(block),
        };
    }

    fn printLocalDeclaration(
        self: *PrettyPrintContext,
        local_declaration: Ast.Statement.LocalDeclaration,
    ) Error!void {
        try self.writer.print("let {s} ", .{local_declaration.name});
        try self.printType(local_declaration.ty);
        try self.writer.writeAll(" = ");
        try self.printExpression(local_declaration.value);
    }

    fn printBlock(self: *PrettyPrintContext, block: Ast.Statement.Block) Error!void {
        if (block.statements.items.len == 0) {
            try self.writer.writeAll("{}");
            return;
        }

        try self.writer.writeByte('{');
        self.indentation += 1;
        for (block.statements.items) |statement| {
            try self.newline();
            try self.printStatement(statement);
        }
        self.indentation -= 1;
        try self.newline();
        try self.writer.writeByte('}');
    }

    fn printExpression(self: *PrettyPrintContext, expression: Ast.Expression) Error!void {
        try switch (expression.data) {
            .integer => |integer| self.printInteger(integer),
        };
    }

    fn printInteger(self: *PrettyPrintContext, integer: u32) Error!void {
        try self.writer.print("{}", .{integer});
    }

    fn newline(self: *PrettyPrintContext) Error!void {
        try self.writer.writeByte('\n');
        try self.writer.writeByteNTimes('\t', self.indentation);
    }
};
