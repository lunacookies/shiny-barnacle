const std = @import("std");

pub fn lex(input: []const u8, allocator: std.mem.Allocator) !std.ArrayList(Token) {
    var lexer = Lexer.init(input, allocator);
    return lexer.lex();
}

pub const Token = struct {
    kind: TokenKind,
    range: TextRange,

    pub fn format(
        self: Token,
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
    func_kw,
    let_kw,
    var_kw,

    identifier,
    integer,
    dot,
    comma,
    semicolon,
    colon,
    equals,
    colon_equals,
    l_paren,
    r_paren,
    l_brace,
    r_brace,
    l_square,
    r_square,

    eof,

    pub fn format(
        self: TokenKind,
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

pub const TextRange = struct {
    start: u32,
    end: u32,

    pub fn format(
        self: TextRange,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{d}..{d}", .{ self.start, self.end });
    }
};

const Lexer = struct {
    tokens: std.ArrayList(Token),
    input: []const u8,
    cursor: u32,

    fn init(input: []const u8, allocator: std.mem.Allocator) Lexer {
        return .{
            .tokens = std.ArrayList(Token).init(allocator),
            .input = input,
            .cursor = 0,
        };
    }

    fn lex(self: *Lexer) !std.ArrayList(Token) {
        while (!self.atEof()) {
            while (self.current() == ' ' or
                self.current() == '\t' or
                self.current() == '\n')
            {
                self.cursor += 1;
            }

            switch (self.current()) {
                '.' => try self.oneByteToken(.dot),
                ',' => try self.oneByteToken(.comma),
                ';' => try self.oneByteToken(.semicolon),
                ':' => try self.oneOrTwoByteToken(.colon, '=', .colon_equals),
                '=' => try self.oneByteToken(.equals),
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

                else => self.emitError(),
            }
        }

        return self.tokens;
    }

    fn oneByteToken(self: *Lexer, kind: TokenKind) !void {
        try self.emit(kind, self.cursor, self.cursor + 1);
        self.cursor += 1;
    }

    fn oneOrTwoByteToken(
        self: *Lexer,
        one_kind: TokenKind,
        two_byte: u8,
        two_kind: TokenKind,
    ) !void {
        if (self.input[self.cursor + 1] != two_byte) {
            try self.oneByteToken(one_kind);
            return;
        }

        try self.emit(two_kind, self.cursor, self.cursor + 2);
        self.cursor += 2;
        return;
    }

    fn emit(self: *Lexer, kind: TokenKind, start: u32, end: u32) !void {
        var token = .{
            .kind = kind,
            .range = .{ .start = start, .end = end },
        };

        if (token.kind != .identifier) {
            try self.tokens.append(token);
            return;
        }

        const KeywordSpec = struct { text: []const u8, kind: TokenKind };
        const keywords = [_]KeywordSpec{
            .{ .text = "func", .kind = .func_kw },
            .{ .text = "let", .kind = .let_kw },
            .{ .text = "var", .kind = .var_kw },
        };

        const token_text = self.input[token.range.start..token.range.end];
        for (keywords) |spec| {
            if (std.mem.eql(u8, token_text, spec.text)) {
                token.kind = spec.kind;
                break;
            }
        }

        try self.tokens.append(token);
    }

    fn emitError(self: *const Lexer) noreturn {
        var line: u32 = 0;
        var column: u32 = 0;

        for (self.input, 0..) |c, i| {
            if (i == self.cursor) break;

            if (c == '\n') {
                line += 1;
                column = 0;
                continue;
            }

            column += 1;
        }

        std.debug.print(
            "{}:{}: error: invalid token\n",
            .{ line + 1, column + 1 },
        );
        std.os.abort();
    }

    fn current(self: *const Lexer) u8 {
        if (self.atEof()) return 0;
        return self.input[self.cursor];
    }

    fn atEof(self: *const Lexer) bool {
        return self.cursor >= self.input.len;
    }
};
