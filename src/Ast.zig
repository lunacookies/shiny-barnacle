const std = @import("std");
const TextRange = @import("TextRange.zig");

const Ast = @This();

items: std.ArrayList(Ast.Item),

pub const Item = struct {
    name: []const u8,
    data: Data,
    range: TextRange,

    pub const Data = union(enum) {
        function: Function,
    };

    pub const Function = struct { body: Ast.Statement };
};

pub const Type = struct {
    name: []const u8,
    range: TextRange,
};

pub const Statement = struct {
    data: Data,
    range: TextRange,

    pub const Data = union(enum) {
        local_declaration: LocalDeclaration,
        return_: Return,
        block: Block,
    };

    pub const LocalDeclaration = struct {
        name: []const u8,
        ty: Ast.Type,
        value: Ast.Expression,
    };

    pub const Return = struct {
        value: Ast.Expression,
    };

    pub const Block = struct {
        statements: std.ArrayList(Ast.Statement),
    };
};

pub const Expression = struct {
    data: Data,
    range: TextRange,

    pub const Data = union(enum) {
        integer: u32,
        binary: Binary,
        name: []const u8,
    };

    pub const Binary = struct {
        lhs: *Expression,
        rhs: *Expression,
        op: Operator,

        pub const Operator = enum {
            add,
            subtract,
            multiply,
            divide,
            modulus,
            bitwise_or,
            bitwise_and,
            xor,
            shift_left,
            shift_right,
            less_than,
            less_than_equal,
            greater_than,
            greater_than_equal,
            equal,
            not_equal,
        };
    };
};

pub fn format(
    self: Ast,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;

    var bounded_array = std.BoundedArray(u8, pretty_print_buf_size).init(0) catch unreachable;

    var ctx = PrettyPrintContext{ .indentation = 0, .writer = bounded_array.writer() };
    ctx.printAst(self) catch unreachable;
    try writer.writeAll(bounded_array.slice());
}

const pretty_print_buf_size = 1024 * 1024;

const PrettyPrintContext = struct {
    indentation: usize,
    writer: std.BoundedArray(u8, pretty_print_buf_size).Writer,

    const Error = error{Overflow};

    fn printAst(self: *PrettyPrintContext, ast: Ast) Error!void {
        for (ast.items.items, 0..) |item, i| {
            if (i != 0) try self.writer.writeAll("\n\n");
            try self.printItem(item);
        }
    }

    fn printItem(self: *PrettyPrintContext, item: Ast.Item) Error!void {
        try switch (item.data) {
            .function => |function| self.printFunction(item.name, function),
        };
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
            .return_ => |return_| self.printReturn(return_),
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

    fn printReturn(
        self: *PrettyPrintContext,
        return_: Ast.Statement.Return,
    ) Error!void {
        try self.writer.writeAll("return ");
        try self.printExpression(return_.value);
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
            .name => |name| self.writer.writeAll(name),

            .binary => |binary| {
                try self.writer.writeByte('(');
                try self.printExpression(binary.lhs.*);
                try self.writer.writeAll(") ");

                const op = switch (binary.op) {
                    .add => "+",
                    .subtract => "-",
                    .multiply => "*",
                    .divide => "/",
                    .modulus => "%",
                    .bitwise_or => "|",
                    .bitwise_and => "&",
                    .xor => "^",
                    .shift_left => "<<",
                    .shift_right => ">>",
                    .less_than => "<",
                    .less_than_equal => "<=",
                    .greater_than => ">",
                    .greater_than_equal => ">=",
                    .equal => "==",
                    .not_equal => "!=",
                };
                try self.writer.writeAll(op);

                try self.writer.writeAll(" (");
                try self.printExpression(binary.rhs.*);
                try self.writer.writeByte(')');
            },
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
