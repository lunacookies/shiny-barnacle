const std = @import("std");

pub fn lex(input: []const u8, allocator: std.mem.Allocator) !std.ArrayList(Token) {
    var lexer = Lexer.init(input, allocator);
    return lexer.lex();
}

pub const Token = struct {
    kind: TokenKind,
    range: TextRange,
};

pub const TokenKind = enum(u8) {
    identifier,
    integer,
    dot,
    comma,
    semicolon,
    colon,
    l_paren,
    r_paren,
    l_brace,
    r_brace,
    l_square,
    r_square,
};

pub const TextRange = struct {
    start: u32,
    end: u32,
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
                '.' => try self.singleCharToken(.dot),
                ',' => try self.singleCharToken(.comma),
                ';' => try self.singleCharToken(.semicolon),
                ':' => try self.singleCharToken(.colon),
                '(' => try self.singleCharToken(.l_paren),
                ')' => try self.singleCharToken(.r_paren),
                '{' => try self.singleCharToken(.l_brace),
                '}' => try self.singleCharToken(.r_brace),
                '[' => try self.singleCharToken(.l_square),
                ']' => try self.singleCharToken(.r_square),

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

                else => std.os.abort(),
            }
        }

        return self.tokens;
    }

    fn singleCharToken(self: *Lexer, kind: TokenKind) !void {
        try self.emit(kind, self.cursor, self.cursor + 1);
        self.cursor += 1;
    }

    fn emit(self: *Lexer, kind: TokenKind, start: u32, end: u32) !void {
        const token = .{
            .kind = kind,
            .range = .{ .start = start, .end = end },
        };
        try self.tokens.append(token);
    }

    fn current(self: *const Lexer) u8 {
        if (self.atEof()) return 0;
        return self.input[self.cursor];
    }

    fn atEof(self: *const Lexer) bool {
        return self.cursor >= self.input.len;
    }
};
