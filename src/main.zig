const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const indexer = @import("indexer.zig");
const codegen = @import("codegen.zig");
const Lir = @import("Lir.zig");

const builtin = @import("builtin");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    var input = std.ArrayList(u8).init(allocator);
    defer input.deinit();
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    const target = switch (builtin.os.tag) {
        .macos => "x86_64-darwin",
        .linux => "x86_64-linux",
        else => {
            try stdout.writeAll("unsupported architecture");
            std.os.exit(92);
        },
    };

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    // Skip the process name
    _ = args.skip();

    if (args.next()) |fname| {
        const file = try std.fs.cwd().openFile(fname, .{});
        const source = try file.readToEndAlloc(allocator, 1 << 28);
        defer allocator.free(source);

        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        // The source file includes the null terminator
        var assembly = try compile(arena.allocator(), source[0 .. source.len - 2], true);
        try assemble(allocator, assembly, "out.s", "out", target);
    } else {
        while (true) {
            try stdout.writeAll("> ");
            const input_string = (try stdin.readUntilDelimiterOrEofAlloc(allocator, '\n', 512)).?;
            if (input_string.len == 0) break;
            defer allocator.free(input_string);

            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();

            var assembly = try compile(arena.allocator(), input_string, true);
            try assemble(allocator, assembly, "out.s", "out", target);
            var out_proc = std.ChildProcess.init(&[_][]const u8{"./out"}, allocator);
            _ = try out_proc.spawnAndWait();
        }
    }
}

pub fn compile(allocator: std.mem.Allocator, in_buf: []const u8, debug: bool) ![]const u8 {
    var tokens = try lexer.lex(in_buf, allocator);
    defer tokens.deinit();

    if (debug) {
        std.debug.print("\n== TOKENS ==\n\n", .{});
        for (tokens.items) |token| {
            std.debug.print("{}\n", .{token});
        }
    }

    var ast = try parser.parse(in_buf, tokens.items, allocator);
    if (debug)
        std.debug.print("\n== AST ==\n\n{}\n", .{ast});

    var file_index = try indexer.indexFile(in_buf, ast, allocator);
    if (debug)
        std.debug.print("\n== INDEX ==\n\n{}\n", .{file_index});

    var lir = try Lir.analyze(
        in_buf,
        file_index,
        ast,
        allocator,
    );
    if (debug)
        std.debug.print("\n== LIR ==\n\n{}\n", .{lir});

    var assembly = try codegen.codegen(in_buf, lir, allocator);
    if (debug)
        std.debug.print("\n== ASM ==\n\n{s}\n", .{assembly});
    return assembly;
}

pub fn assemble(
    allocator: std.mem.Allocator,
    assembly: []const u8,
    asm_fname: []const u8,
    exe_fname: []const u8,
    target: []const u8,
) !void {
    var assembly_file = try std.fs.cwd().createFile(asm_fname, .{});
    try assembly_file.writeAll(assembly);

    var clang = std.ChildProcess.init(&[_][]const u8{
        "clang",
        "-target",
        target,
        "-o",
        exe_fname,
        asm_fname,
    }, allocator);

    _ = try clang.spawnAndWait();
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
