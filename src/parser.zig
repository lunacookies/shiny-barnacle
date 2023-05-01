const std = @import("std");
const lexer = @import("lexer.zig");

pub fn parse(input: []const u8, tokens: []const lexer.Token, allocator: std.mem.Allocator) !Ast {
    var parser = Parser.init(input, tokens);
    return parser.parse(allocator);
}

pub const Ast = struct {
    items: std.ArrayList(Ast.Item),

    pub const Item = union(enum) {
        function: Function,

        pub const Function = struct {
            name: []const u8,
            body: Ast.Statement,

            pub fn format(
                self: Ast.Item.Function,
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                _ = fmt;
                _ = options;
                try writer.print("func {s} {}", .{ self.name, self.body });
            }
        };

        pub fn format(
            self: Ast.Item,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;
            switch (self) {
                .function => |function| try writer.print("{}", .{function}),
            }
        }
    };

    pub const Statement = union(enum) {
        local_declaration: LocalDeclaration,

        pub const LocalDeclaration = struct {
            name: []const u8,
            value: Ast.Expression,

            pub fn format(
                self: Ast.Statement.LocalDeclaration,
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                _ = fmt;
                _ = options;
                try writer.print("{s} := {}", .{ self.name, self.value });
            }
        };

        pub fn format(
            self: Ast.Statement,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;
            switch (self) {
                .local_declaration => |ld| try writer.print("{}", .{ld}),
            }
        }
    };

    pub const Expression = union(enum) {
        integer: u32,

        pub fn format(
            self: Ast.Expression,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;
            switch (self) {
                .integer => |integer| try writer.print("{}", .{integer}),
            }
        }
    };

    pub fn format(
        self: Ast,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        for (self.items.items, 0..) |item, i| {
            if (i != 0) try writer.print("\n\n", .{});
            try writer.print("{}", .{item});
        }
    }
};

const Parser = struct {
    input: []const u8,
    tokens: []const lexer.Token,
    cursor: usize,

    fn init(input: []const u8, tokens: []const lexer.Token) Parser {
        return .{ .input = input, .tokens = tokens, .cursor = 0 };
    }

    fn parse(self: *Parser, allocator: std.mem.Allocator) !Ast {
        var items = std.ArrayList(Ast.Item).init(allocator);

        while (!self.atEof()) {
            const item = try self.parseItem(allocator);
            try items.append(item);
        }

        return .{ .items = items };
    }

    fn parseItem(self: *Parser, allocator: std.mem.Allocator) !Ast.Item {
        switch (self.current()) {
            .func_kw => return self.parseFunction(allocator),
            else => self.emitError("expected item", .{}),
        }
    }

    fn parseFunction(self: *Parser, allocator: std.mem.Allocator) !Ast.Item {
        self.bump(.func_kw);
        const name = self.expectWithText(.identifier);
        const body = try self.parseStatement(allocator);

        return .{ .function = .{ .name = name, .body = body } };
    }

    fn parseStatement(self: *Parser, allocator: std.mem.Allocator) !Ast.Statement {
        switch (self.current()) {
            .identifier => {
                if (self.lookahead() == .colon_equals) {
                    return self.parseLocalDeclaration(allocator);
                }
                self.emitError("expected statement", .{});
            },
            else => self.emitError("expected statement", .{}),
        }
    }

    fn parseLocalDeclaration(self: *Parser, allocator: std.mem.Allocator) !Ast.Statement {
        const name = self.bumpWithText(.identifier);
        self.bump(.colon_equals);
        const value = try self.parseExpression(allocator);
        return .{ .local_declaration = .{ .name = name, .value = value } };
    }

    fn parseExpression(self: *Parser, allocator: std.mem.Allocator) !Ast.Expression {
        _ = allocator;
        switch (self.current()) {
            .integer => {
                self.bump(.integer);
                return .{ .integer = 92 };
            },
            else => self.emitError("expected expression", .{}),
        }
    }

    fn expect(self: *Parser, kind: lexer.TokenKind) void {
        _ = self.expectWithText(kind);
    }

    fn expectWithText(self: *Parser, kind: lexer.TokenKind) []const u8 {
        if (self.current() != kind) {
            self.emitError("expected {} but found {}", .{ kind, self.current() });
        }

        return self.bumpWithText(kind);
    }

    fn emitError(self: *const Parser, comptime fmt: []const u8, args: anytype) noreturn {
        _ = self;
        std.debug.print("error: ", .{});
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});
        std.os.abort();
    }

    fn bump(self: *Parser, kind: lexer.TokenKind) void {
        _ = self.bumpWithText(kind);
    }

    fn bumpWithText(self: *Parser, kind: lexer.TokenKind) []const u8 {
        std.debug.assert(self.current() == kind);
        const range = self.tokens[self.cursor].range;
        const text = self.input[range.start..range.end];
        self.cursor += 1;
        return text;
    }

    fn current(self: *const Parser) lexer.TokenKind {
        if (self.atEof()) return .eof;
        return self.tokens[self.cursor].kind;
    }

    fn lookahead(self: *const Parser) lexer.TokenKind {
        if (self.cursor + 1 >= self.tokens.len) return .eof;
        return self.tokens[self.cursor + 1].kind;
    }

    fn atEof(self: *const Parser) bool {
        return self.cursor >= self.tokens.len;
    }
};
