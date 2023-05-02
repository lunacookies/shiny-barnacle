const std = @import("std");
const utils = @import("utils.zig");
const Ast = @import("Ast.zig");
const Allocator = std.mem.Allocator;
const TextRange = @import("TextRange.zig");

pub fn indexFile(
    input: []const u8,
    ast: Ast,
    a: Allocator,
) !FileIndex {
    var items = std.StringHashMap(Item).init(a);

    for (ast.items.items) |ast_item| {
        const item_data = switch (ast_item.data) {
            .function => .{ .function = .{} },
        };

        const item = .{
            .data = item_data,
            .range = ast_item.range,
        };

        if (items.contains(ast_item.name)) {
            const line_col = utils.indexToLineCol(input, ast_item.range.start);

            std.debug.print(
                "{}:{}: error: an item called “{s}” has already been defined\n",
                .{
                    line_col.line + 1,
                    line_col.column + 1,
                    ast_item.name,
                },
            );
            std.os.exit(92);
        }

        try items.put(ast_item.name, item);
    }

    return .{ .items = items };
}

const FileIndex = struct {
    items: std.StringHashMap(Item),

    pub fn format(
        self: FileIndex,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        var bounded_array = std.BoundedArray(u8, pretty_print_buf_size).init(0) catch unreachable;

        var ctx = PrettyPrintContext{ .writer = bounded_array.writer() };
        ctx.printFileIndex(self) catch unreachable;
        try writer.writeAll(bounded_array.slice());
    }
};

pub const Item = struct {
    data: Data,
    range: TextRange,

    pub const Data = union(enum) {
        function: Function,
    };

    pub const Function = struct {};
};

const pretty_print_buf_size = 1024 * 1024;

const PrettyPrintContext = struct {
    writer: std.BoundedArray(u8, pretty_print_buf_size).Writer,

    const Error = error{Overflow};

    fn printFileIndex(self: *PrettyPrintContext, file_index: FileIndex) Error!void {
        var iterator = file_index.items.iterator();
        var i: u32 = 0;
        while (iterator.next()) |entry| : (i += 1) {
            const name = entry.key_ptr.*;
            const item = entry.value_ptr.*;

            if (i != 0) try self.writer.writeAll("\n");
            try self.printItem(name, item);
        }
    }

    fn printItem(self: *PrettyPrintContext, name: []const u8, item: Item) Error!void {
        try switch (item.data) {
            .function => |function| self.printFunction(name, function),
        };
    }

    fn printFunction(
        self: *PrettyPrintContext,
        name: []const u8,
        function: Item.Function,
    ) Error!void {
        _ = function;
        try self.writer.print("func {s}", .{name});
    }
};
