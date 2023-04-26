const std = @import("std");

pub fn lex(input: []const u8, allocator: std.mem.Allocator) !std.ArrayList(Token) {
    var lexer = Lexer.init(input, allocator);
    return lexer.lex();
}

pub const Token = struct {
    kind: TokenKind,
    input: []const u8,
    range: TextRange,
};

pub const TokenKind = enum(u8) {
    identifier,
    integer,
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

            if ('0' <= self.current() and self.current() <= '9') {
                const start = self.cursor;
                while ('0' <= self.current() and self.current() <= '9')
                    self.cursor += 1;
                const end = self.cursor;
                try self.emit(.integer, start, end);
                continue;
            }

            if ('a' <= self.current() and self.current() <= 'z') {
                const start = self.cursor;
                while (('a' <= self.current() and self.current() <= 'z') or
                    ('0' <= self.current() and self.current() <= '9') or
                    (self.current() == '_'))
                {
                    self.cursor += 1;
                }
                const end = self.cursor;
                try self.emit(.identifier, start, end);
                continue;
            }

            std.os.abort();
        }

        return self.tokens;
    }

    fn emit(self: *Lexer, kind: TokenKind, start: u32, end: u32) !void {
        const token = .{
            .kind = kind,
            .input = self.input[start..end],
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
