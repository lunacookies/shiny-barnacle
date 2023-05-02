const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const indexer = @import("indexer.zig");
const Lir = @import("Lir.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var input = std.ArrayList(u8).init(allocator);
    defer input.deinit();
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    while (true) {
        try stdout.writeAll("> ");
        try stdin.readUntilDelimiterArrayList(&input, '\n', 512);
        defer input.clearRetainingCapacity();

        var tokens = try lexer.lex(input.items, allocator);
        defer tokens.deinit();

        for (tokens.items) |token| {
            std.debug.print("{}\n", .{token});
        }

        var ast = try parser.parse(input.items, tokens.items, allocator);
        std.debug.print("{}\n", .{ast});

        var file_index = try indexer.indexFile(input.items, ast, allocator);
        std.debug.print("{}\n", .{file_index});

        var lir = try Lir.analyze(
            input.items,
            file_index,
            ast,
            allocator,
        );
        std.debug.print("{}\n", .{lir});
    }
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
