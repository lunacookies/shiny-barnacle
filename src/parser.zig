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
        while (self.current() == .semicolon) self.bump(.semicolon);
        return switch (self.current()) {
            .let_kw => self.parseLocalDeclaration(a),
            .return_kw => self.parseReturn(a),
            .if_kw => self.parseIf(a),
            .while_kw => self.parseWhile(a),
            .l_brace => self.parseBlock(a),
            else => self.parseAssignOrExprStmt(a),
        };
    }

    fn parseAssignOrExprStmt(self: *Parser, a: Allocator) !Ast.Statement {
        const start = self.inputIndex();
        const lhs = try self.parseExpression(a);
        if (self.current() == .equals) {
            self.bump(.equals);
            const value = try self.parseExpression(a);
            const end = self.inputIndex();
            return .{
                .data = .{ .assign = .{
                    .lhs = lhs,
                    .rhs = value,
                } },
                .range = .{ .start = start, .end = end },
            };
        } else {
            const end = self.inputIndex();
            return .{
                .data = .{
                    .expr = lhs,
                },
                .range = .{ .start = start, .end = end },
            };
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

    fn parseReturn(self: *Parser, a: Allocator) !Ast.Statement {
        const start = self.inputIndex();
        self.bump(.return_kw);
        const value = try self.parseExpression(a);
        const end = self.inputIndex();

        return .{
            .data = .{
                .return_ = .{ .value = value },
            },
            .range = .{ .start = start, .end = end },
        };
    }

    fn parseIf(self: *Parser, a: Allocator) !Ast.Statement {
        const start = self.inputIndex();
        self.bump(.if_kw);

        const condition = try self.parseExpression(a);

        const true_branch = try a.create(Ast.Statement);
        true_branch.* = try self.parseBlock(a);

        var false_branch: ?*Ast.Statement = null;
        if (self.current() == .else_kw) {
            self.bump(.else_kw);
            const p = try a.create(Ast.Statement);
            p.* = try self.parseBlock(a);
            false_branch = p;
        }

        const end = self.inputIndex();

        return .{
            .data = .{
                .if_ = .{
                    .condition = condition,
                    .true_branch = true_branch,
                    .false_branch = false_branch,
                },
            },
            .range = .{ .start = start, .end = end },
        };
    }

    fn parseWhile(self: *Parser, a: Allocator) !Ast.Statement {
        const start = self.inputIndex();
        self.bump(.while_kw);

        const condition = try self.parseExpression(a);

        const body = try a.create(Ast.Statement);
        body.* = try self.parseBlock(a);

        const end = self.inputIndex();

        return .{
            .data = .{
                .while_ = .{
                    .condition = condition,
                    .body = body,
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
        return self.parseExpressionWithBindingPower(0, a);
    }

    fn parseExpressionWithBindingPower(
        self: *Parser,
        min_binding_power: u8,
        a: Allocator,
    ) !Ast.Expression {
        const OpBindingPower = struct {
            op: Ast.Expression.Binary.Operator,
            binding_power: u8,
        };

        var lhs = try self.parseLhs(a);

        while (true) {
            const op_binding_power: OpBindingPower = switch (self.current()) {
                .asterisk => .{
                    .op = .multiply,
                    .binding_power = 8,
                },
                .slash => .{
                    .op = .divide,
                    .binding_power = 8,
                },
                .percent => .{
                    .op = .modulus,
                    .binding_power = 8,
                },

                .plus => .{
                    .op = .add,
                    .binding_power = 7,
                },
                .hyphen => .{
                    .op = .subtract,
                    .binding_power = 7,
                },

                .less_than_less_than => .{
                    .op = .shift_left,
                    .binding_power = 6,
                },
                .greater_than_greater_than => .{
                    .op = .shift_right,
                    .binding_power = 6,
                },

                .less_than => .{
                    .op = .less_than,
                    .binding_power = 5,
                },
                .less_than_equals => .{
                    .op = .less_than_equal,
                    .binding_power = 5,
                },
                .greater_than => .{
                    .op = .greater_than,
                    .binding_power = 5,
                },
                .greater_than_equals => .{
                    .op = .greater_than_equal,
                    .binding_power = 5,
                },

                .equals_equals => .{
                    .op = .equal,
                    .binding_power = 4,
                },
                .exclamation_equals => .{
                    .op = .not_equal,
                    .binding_power = 4,
                },

                .ampersand => .{
                    .op = .bitwise_and,
                    .binding_power = 3,
                },

                .caret => .{
                    .op = .xor,
                    .binding_power = 2,
                },

                .pipe => .{
                    .op = .bitwise_or,
                    .binding_power = 1,
                },

                else => break,
            };

            if (op_binding_power.binding_power < min_binding_power) {
                break;
            }

            self.bumpAny(); // eat operator

            const rhs = try self.parseExpressionWithBindingPower(
                op_binding_power.binding_power + 1,
                a,
            );

            const lhs_p = try a.create(Ast.Expression);
            const rhs_p = try a.create(Ast.Expression);
            lhs_p.* = lhs;
            rhs_p.* = rhs;

            lhs = .{
                .data = .{
                    .binary = .{
                        .lhs = lhs_p,
                        .rhs = rhs_p,
                        .op = op_binding_power.op,
                    },
                },
                .range = .{ .start = lhs.range.start, .end = rhs.range.end },
            };
        }

        return lhs;
    }

    fn parseLhs(self: *Parser, a: Allocator) !Ast.Expression {
        switch (self.current()) {
            .integer => {
                const start = self.inputIndex();
                const text = self.bumpWithText(.integer);
                const end = self.inputIndex();

                return .{
                    .data = .{ .integer = text },
                    .range = .{ .start = start, .end = end },
                };
            },

            .l_paren => {
                self.bump(.l_paren);
                const inner = self.parseExpression(a);
                self.expect(.r_paren);

                return inner;
            },

            .identifier => {
                const start = self.inputIndex();
                const name = self.expectWithText(.identifier);
                const end = self.inputIndex();
                return .{
                    .data = .{ .name = name },
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

        std.debug.print("{}: error: ", .{line_col});
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
        return self.bumpAnyWithText();
    }

    fn bumpAny(self: *Parser) void {
        _ = self.bumpAnyWithText();
    }

    fn bumpAnyWithText(self: *Parser) []const u8 {
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
