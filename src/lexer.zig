const std = @import("std");
const utils = @import("utils.zig");
const TextRange = @import("TextRange.zig");
const Allocator = std.mem.Allocator;
const ALU = std.ArrayListUnmanaged;

pub fn lex(input: []const u8, allocator: Allocator) !ALU(Token) {
    var lexer = Lexer.init(input, allocator);
    return lexer.lex();
}

pub const Token = struct {
    const Self = @This();

    kind: TokenKind,
    range: TextRange,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{}@{}", .{ self.kind, self.range });
    }
};

pub const TokenKind = enum(u8) {
    const Self = @This();

    func_kw,
    struct_kw,
    if_kw,
    else_kw,
    while_kw,
    let_kw,
    var_kw,
    and_kw,
    or_kw,
    return_kw,

    identifier,
    integer,
    dot,
    comma,
    semicolon,
    colon,
    equals,
    equals_equals,
    colon_equals,
    plus,
    hyphen,
    asterisk,
    slash,
    percent,
    pipe,
    ampersand,
    caret,
    tilde,
    exclamation,
    exclamation_equals,
    less_than,
    less_than_equals,
    less_than_less_than,
    greater_than,
    greater_than_equals,
    greater_than_greater_than,
    l_paren,
    r_paren,
    l_brace,
    r_brace,
    l_square,
    r_square,

    eof,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        const name = @tagName(self);
        try writer.writeAll(name);
    }
};

const Lexer = struct {
    const Self = @This();

    tokens: ALU(Token),
    allocator: Allocator,
    input: []const u8,
    cursor: u32,

    const keywords = std.ComptimeStringMap(TokenKind, .{
        .{ "func", .func_kw },
        .{ "struct", .struct_kw },
        .{ "if", .if_kw },
        .{ "else", .else_kw },
        .{ "while", .while_kw },
        .{ "let", .let_kw },
        .{ "var", .var_kw },
        .{ "return", .return_kw },
    });

    fn init(input: []const u8, allocator: Allocator) Lexer {
        return .{
            .tokens = ALU(Token){},
            .allocator = allocator,
            .input = input,
            .cursor = 0,
        };
    }

    fn lex(self: *Self) !ALU(Token) {
        while (!self.atEof()) {
            self.skipWhitespace();

            switch (self.current()) {
                '.' => try self.oneByteToken(.dot),
                ',' => try self.oneByteToken(.comma),
                ';' => try self.oneByteToken(.semicolon),
                ':' => try self.oneOrTwoByteToken(
                    .colon,
                    &[_]ByteAndKind{
                        .{ .byte = '=', .kind = .colon_equals },
                    },
                ),
                '=' => try self.oneOrTwoByteToken(
                    .equals,
                    &[_]ByteAndKind{
                        .{ .byte = '=', .kind = .equals_equals },
                    },
                ),
                '+' => try self.oneByteToken(.plus),
                '-' => try self.oneByteToken(.hyphen),
                '*' => try self.oneByteToken(.asterisk),
                '/' => try self.oneByteToken(.slash),
                '%' => try self.oneByteToken(.percent),
                '|' => try self.oneByteToken(.pipe),
                '&' => try self.oneByteToken(.ampersand),
                '^' => try self.oneByteToken(.caret),
                '~' => try self.oneByteToken(.tilde),
                '!' => try self.oneOrTwoByteToken(
                    .exclamation,
                    &[_]ByteAndKind{
                        .{ .byte = '=', .kind = .exclamation_equals },
                    },
                ),
                '<' => try self.oneOrTwoByteToken(
                    .less_than,
                    &[_]ByteAndKind{
                        .{ .byte = '=', .kind = .less_than_equals },
                        .{ .byte = '<', .kind = .less_than_less_than },
                    },
                ),
                '>' => try self.oneOrTwoByteToken(
                    .greater_than,
                    &[_]ByteAndKind{
                        .{ .byte = '=', .kind = .greater_than_equals },
                        .{ .byte = '>', .kind = .greater_than_greater_than },
                    },
                ),
                '(' => try self.oneByteToken(.l_paren),
                ')' => try self.oneByteToken(.r_paren),
                '{' => try self.oneByteToken(.l_brace),
                '}' => try self.oneByteToken(.r_brace),
                '[' => try self.oneByteToken(.l_square),
                ']' => try self.oneByteToken(.r_square),

                '0'...'9' => {
                    const start = self.cursor;
                    while ('0' <= self.current() and self.current() <= '9')
                        self.cursor += 1;
                    const end = self.cursor;
                    try self.emit(.integer, start, end);
                },

                'a'...'z' => {
                    const start = self.cursor;
                    while (('a' <= self.current() and self.current() <= 'z') or
                        ('0' <= self.current() and self.current() <= '9') or
                        (self.current() == '_'))
                    {
                        self.cursor += 1;
                    }
                    const end = self.cursor;
                    try self.emit(.identifier, start, end);
                },
                0 => break,
                else => |c| self.emitError(c),
            }
        }

        return self.tokens;
    }

    fn skipWhitespace(self: *Self) void {
        while (self.current() == ' ' or
            self.current() == '\t' or
            self.current() == '\n')
        {
            self.cursor += 1;
        }
    }

    fn oneByteToken(self: *Self, kind: TokenKind) !void {
        try self.emit(kind, self.cursor, self.cursor + 1);
        self.cursor += 1;
    }

    const ByteAndKind = struct { byte: u8, kind: TokenKind };

    fn oneOrTwoByteToken(
        self: *Self,
        one_kind: TokenKind,
        two_byte_and_kinds: []const ByteAndKind,
    ) !void {
        for (two_byte_and_kinds) |byte_and_kind| {
            if (self.input[self.cursor + 1] == byte_and_kind.byte) {
                try self.emit(byte_and_kind.kind, self.cursor, self.cursor + 2);
                self.cursor += 2;
                return;
            }
        }

        try self.oneByteToken(one_kind);
    }

    fn emit(self: *Self, kind: TokenKind, start: u32, end: u32) !void {
        var token = .{
            .kind = kind,
            .range = .{ .start = start, .end = end },
        };

        if (token.kind != .identifier) {
            try self.tokens.append(self.allocator, token);
            return;
        }

        const token_text = self.input[token.range.start..token.range.end];
        token.kind = keywords.get(token_text) orelse .identifier;

        try self.tokens.append(self.allocator, token);
    }

    fn emitError(self: *const Self, c: u8) noreturn {
        const line_col = utils.indexToLineCol(self.input, self.cursor);
        std.debug.print("{}: error: invalid token (code: {})\n", .{ line_col, c });
        std.os.exit(92);
    }

    fn current(self: *const Self) u8 {
        if (self.atEof()) return 0;
        return self.input[self.cursor];
    }

    fn atEof(self: *const Self) bool {
        return self.cursor >= self.input.len;
    }
};
