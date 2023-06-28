const std = @import("std");
const TextRange = @import("TextRange.zig");

const Ast = @This();

items: []const Ast.Item,

pub const Item = struct {
    name: []const u8,
    data: Data,
    range: TextRange,

    pub const Data = union(enum) {
        function: Function,
        strukt: Struct,
    };

    pub const Function = struct {
        body: Ast.Statement,
        params: []const Param,
        return_type: Type,

        pub const Param = struct {
            name: []const u8,
            ty: Type,
            range: TextRange,
        };
    };

    pub const Struct = struct {
        fields: []const Field,

        pub const Field = struct {
            name: []const u8,
            ty: Type,
            range: TextRange,
        };
    };
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
        assign: Assign,
        expr: Expression,
        return_: Return,
        if_: If,
        while_: While,
        block: Block,
    };

    pub const LocalDeclaration = struct {
        name: []const u8,
        ty: Ast.Type,
        value: ?Ast.Expression,
    };

    pub const Assign = struct {
        lhs: Ast.Expression,
        rhs: Ast.Expression,
    };

    pub const Return = struct {
        value: ?Ast.Expression,
    };

    pub const If = struct {
        condition: Ast.Expression,
        true_branch: *const Ast.Statement,
        false_branch: ?*const Ast.Statement,
    };

    pub const While = struct {
        condition: Ast.Expression,
        body: *const Ast.Statement,
    };

    pub const Block = struct {
        statements: []const Ast.Statement,
    };
};

pub const Expression = struct {
    data: Data,
    range: TextRange,

    pub const Data = union(enum) {
        integer: []const u8,
        unary: Unary,
        binary: Binary,
        name: []const u8,
        call: Call,
    };

    pub const Call = struct {
        lhs: *const Expression,
        params: []const Expression,
    };

    pub const Unary = struct {
        operand: *const Expression,
        op: Operator,

        pub const Operator = enum {
            dereference,
            address_of,
        };
    };

    pub const Binary = struct {
        lhs: *const Expression,
        rhs: *const Expression,
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
    const Self = @This();

    indentation: usize,
    writer: std.BoundedArray(u8, pretty_print_buf_size).Writer,

    const Error = error{Overflow};

    fn printAst(self: *Self, ast: Ast) Error!void {
        for (ast.items, 0..) |item, i| {
            if (i != 0) try self.writer.writeAll("\n\n");
            try self.printItem(item);
        }
    }

    fn printItem(self: *Self, item: Ast.Item) Error!void {
        try switch (item.data) {
            .function => |function| self.printFunction(item.name, function),
            .strukt => |strukt| self.printStruct(item.name, strukt),
        };
    }

    fn printFunction(
        self: *Self,
        name: []const u8,
        function: Ast.Item.Function,
    ) Error!void {
        try self.writer.print("func {s} ", .{name});
        try self.printStatement(function.body);
    }

    fn printStruct(
        self: *Self,
        name: []const u8,
        strukt: Ast.Item.Struct,
    ) Error!void {
        try self.writer.print("struct {s} {{", .{name});
        for (strukt.fields) |field| {
            try self.writer.print("\n\t{s} ", .{field.name});
            try self.printType(field.ty);
            try self.writer.writeByte(',');
        }
        try self.writer.writeAll("\n}");
    }

    fn printType(self: *Self, ty: Ast.Type) Error!void {
        try self.writer.writeAll(ty.name);
    }

    fn printStatement(self: *Self, statement: Ast.Statement) Error!void {
        try switch (statement.data) {
            .local_declaration => |ld| self.printLocalDeclaration(ld),
            .return_ => |return_| self.printReturn(return_),
            .if_ => |if_| self.printIf(if_),
            .while_ => |while_| self.printWhile(while_),
            .block => |block| self.printBlock(block),
            .expr => |expr| self.printExpression(expr),
            .assign => |assign| self.printAssign(assign),
        };
    }

    fn printLocalDeclaration(
        self: *Self,
        local_declaration: Ast.Statement.LocalDeclaration,
    ) Error!void {
        try self.writer.print("let {s} ", .{local_declaration.name});
        try self.printType(local_declaration.ty);
        if (local_declaration.value) |value| {
            try self.writer.writeAll(" = ");
            try self.printExpression(value);
        }
    }

    fn printAssign(
        self: *Self,
        assign: Ast.Statement.Assign,
    ) Error!void {
        try self.printExpression(assign.lhs);
        try self.writer.writeAll(" = ");
        try self.printExpression(assign.rhs);
    }

    fn printReturn(
        self: *Self,
        return_: Ast.Statement.Return,
    ) Error!void {
        try self.writer.writeAll("return ");
        if (return_.value) |val|
            try self.printExpression(val);
    }

    fn printIf(self: *Self, if_: Ast.Statement.If) Error!void {
        try self.writer.writeAll("if ");
        try self.printExpression(if_.condition);
        try self.writer.writeAll(" ");
        try self.printStatement(if_.true_branch.*);
        if (if_.false_branch) |false_branch| {
            try self.writer.writeAll(" else ");
            try self.printStatement(false_branch.*);
        }
    }

    fn printWhile(self: *Self, while_: Ast.Statement.While) Error!void {
        try self.writer.writeAll("while ");
        try self.printExpression(while_.condition);
        try self.writer.writeAll(" ");
        try self.printStatement(while_.body.*);
    }

    fn printBlock(self: *Self, block: Ast.Statement.Block) Error!void {
        if (block.statements.len == 0) {
            try self.writer.writeAll("{}");
            return;
        }

        try self.writer.writeByte('{');
        self.indentation += 1;
        for (block.statements) |statement| {
            try self.newline();
            try self.printStatement(statement);
        }
        self.indentation -= 1;
        try self.newline();
        try self.writer.writeByte('}');
    }

    fn printExpression(self: *Self, expression: Ast.Expression) Error!void {
        try switch (expression.data) {
            .integer => |integer| self.writer.writeAll(integer),
            .name => |name| self.writer.writeAll(name),

            .unary => |unary| {
                const op: u8 = switch (unary.op) {
                    .dereference => '*',
                    .address_of => '&',
                };
                try self.writer.writeByte(op);
                try self.writer.writeByte('(');
                try self.printExpression(unary.operand.*);
                try self.writer.writeByte(')');
            },

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
            .call => |call| {
                try self.printExpression(call.lhs.*);
                try self.writer.writeByte('(');
                for (call.params, 0..) |param, i| {
                    if (i != 0) try self.writer.writeAll(", ");
                    try self.printExpression(param);
                }
                try self.writer.writeByte(')');
            },
        };
    }

    fn printInteger(self: *Self, integer: u32) Error!void {
        try self.writer.print("{}", .{integer});
    }

    fn newline(self: *Self) Error!void {
        try self.writer.writeByte('\n');
        try self.writer.writeByteNTimes('\t', self.indentation);
    }
};
