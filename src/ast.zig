const std = @import("std");
const Ast = @This();

items: std.ArrayList(Ast.Item),

pub const Item = union(enum) {
    function: Function,

    pub const Function = struct {
        name: []const u8,
        body: Ast.Statement,
    };
};

pub const Statement = union(enum) {
    local_declaration: LocalDeclaration,
    block: Block,

    pub const LocalDeclaration = struct {
        name: []const u8,
        value: Ast.Expression,
    };

    pub const Block = struct {
        statements: std.ArrayList(Ast.Statement),
    };
};

pub const Expression = union(enum) {
    integer: u32,
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
        try switch (item) {
            .function => |function| self.printFunction(function),
        };
    }

    fn printFunction(self: *PrettyPrintContext, function: Ast.Item.Function) Error!void {
        try self.writer.print("func {s} ", .{function.name});
        try self.printStatement(function.body);
    }

    fn printStatement(self: *PrettyPrintContext, statement: Ast.Statement) Error!void {
        try switch (statement) {
            .local_declaration => |ld| self.printLocalDeclaration(ld),
            .block => |block| self.printBlock(block),
        };
    }

    fn printLocalDeclaration(
        self: *PrettyPrintContext,
        local_declaration: Ast.Statement.LocalDeclaration,
    ) Error!void {
        try self.writer.print("{s} := ", .{local_declaration.name});
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
        try switch (expression) {
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
