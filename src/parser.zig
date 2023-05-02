const std = @import("std");
const lexer = @import("lexer.zig");
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
        self.bump(.func_kw);
        const name = self.expectWithText(.identifier);
        const body = try self.parseBlock(a);

        return .{ .function = .{ .name = name, .body = body } };
    }

    fn parseStatement(self: *Parser, a: Allocator) Allocator.Error!Ast.Statement {
        switch (self.current()) {
            .identifier => {
                if (self.lookahead() == .colon_equals) {
                    return self.parseLocalDeclaration(a);
                }
                self.emitError("expected statement", .{});
            },
            .l_brace => return self.parseBlock(a),
            else => self.emitError("expected statement", .{}),
        }
    }

    fn parseLocalDeclaration(self: *Parser, a: Allocator) !Ast.Statement {
        const name = self.bumpWithText(.identifier);
        self.bump(.colon_equals);
        const value = try self.parseExpression(a);
        return .{ .local_declaration = .{ .name = name, .value = value } };
    }

    fn parseBlock(self: *Parser, a: Allocator) !Ast.Statement {
        self.expect(.l_brace);
        var statements = std.ArrayList(Ast.Statement).init(a);
        while (!self.atEof() and self.current() != .r_brace) {
            const statement = try self.parseStatement(a);
            try statements.append(statement);
        }
        self.expect(.r_brace);
        return .{ .block = .{ .statements = statements } };
    }

    fn parseExpression(self: *Parser, a: Allocator) !Ast.Expression {
        _ = a;
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
        var line: u32 = 0;
        var column: u32 = 0;
        var cursor_idx = if (self.atEof())
            self.tokens[self.tokens.len - 1].range.end
        else
            self.tokens[self.cursor].range.start;

        for (self.input, 0..) |c, i| {
            if (i == cursor_idx) break;

            if (c == '\n') {
                line += 1;
                column = 0;
                continue;
            }

            column += 1;
        }

        std.debug.print(
            "{}:{}: error: ",
            .{ line + 1, column + 1 },
        );
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
