const std = @import("std");
const lexer = @import("lexer.zig");
const utils = @import("utils.zig");
const Ast = @import("Ast.zig");
const Allocator = std.mem.Allocator;

pub fn parse(input: []const u8, tokens: []const lexer.Token, a: Allocator) !Ast {
    var parser = Parser.init(input, tokens);
    return parser.parse(a);
}

const Parser = struct {
    input: []const u8,
    tokens: []const lexer.Token,
    cursor: usize,

    fn init(input: []const u8, tokens: []const lexer.Token) Parser {
        return .{ .input = input, .tokens = tokens, .cursor = 0 };
    }

    fn parse(self: *Parser, a: Allocator) !Ast {
        var items = std.ArrayList(Ast.Item).init(a);

        while (!self.atEof()) {
            const item = try self.parseItem(a);
            try items.append(item);
        }

        return .{ .items = items };
    }

    fn parseItem(self: *Parser, a: Allocator) !Ast.Item {
        switch (self.current()) {
            .func_kw => return self.parseFunction(a),
            else => self.emitError("expected item", .{}),
        }
    }

    fn parseFunction(self: *Parser, a: Allocator) !Ast.Item {
        const start = self.inputIndex();

        self.bump(.func_kw);
        const name = self.expectWithText(.identifier);
        const body = try self.parseBlock(a);

        const end = self.inputIndex();

        return .{
            .name = name,
            .data = .{
                .function = .{ .body = body },
            },
            .range = .{ .start = start, .end = end },
        };
    }

    fn parseType(self: *Parser) Ast.Type {
        switch (self.current()) {
            .identifier => {
                const start = self.inputIndex();
                const name = self.expectWithText(.identifier);
                const end = self.inputIndex();
                return .{
                    .name = name,
                    .range = .{ .start = start, .end = end },
                };
            },
            else => self.emitError("expected type", .{}),
        }
    }

    fn parseStatement(self: *Parser, a: Allocator) Allocator.Error!Ast.Statement {
        switch (self.current()) {
            .let_kw => return self.parseLocalDeclaration(a),
            .l_brace => return self.parseBlock(a),
            else => self.emitError("expected statement", .{}),
        }
    }

    fn parseLocalDeclaration(self: *Parser, a: Allocator) !Ast.Statement {
        const start = self.inputIndex();

        self.bump(.let_kw);
        const name = self.bumpWithText(.identifier);
        const ty = self.parseType();
        self.bump(.equals);
        const value = try self.parseExpression(a);

        const end = self.inputIndex();

        return .{
            .data = .{
                .local_declaration = .{
                    .name = name,
                    .ty = ty,
                    .value = value,
                },
            },
            .range = .{ .start = start, .end = end },
        };
    }

    fn parseBlock(self: *Parser, a: Allocator) !Ast.Statement {
        const start = self.inputIndex();

        self.expect(.l_brace);
        var statements = std.ArrayList(Ast.Statement).init(a);
        while (!self.atEof() and self.current() != .r_brace) {
            const statement = try self.parseStatement(a);
            try statements.append(statement);
        }
        self.expect(.r_brace);

        const end = self.inputIndex();

        return .{
            .data = .{ .block = .{ .statements = statements } },
            .range = .{ .start = start, .end = end },
        };
    }

    fn parseExpression(self: *Parser, a: Allocator) !Ast.Expression {
        _ = a;
        switch (self.current()) {
            .integer => {
                const start = self.inputIndex();
                const text = self.bumpWithText(.integer);
                const end = self.inputIndex();

                const i = std.fmt.parseInt(u32, text, 10) catch unreachable;

                return .{
                    .data = .{ .integer = i },
                    .range = .{ .start = start, .end = end },
                };
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
        const line_col = utils.indexToLineCol(self.input, self.inputIndex());

        std.debug.print(
            "{}:{}: error: ",
            .{ line_col.line + 1, line_col.column + 1 },
        );
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});
        std.os.exit(92);
    }

    fn inputIndex(self: *const Parser) u32 {
        if (self.atEof()) {
            return self.tokens[self.tokens.len - 1].range.end;
        } else {
            return self.tokens[self.cursor].range.start;
        }
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
