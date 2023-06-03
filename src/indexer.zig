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
            .function => Item.Data{ .function = .{} },
            .strukt => |strukt| try lowerStruct(strukt, input, a),
        };

        const item = .{
            .data = item_data,
            .range = ast_item.range,
        };

        if (items.contains(ast_item.name)) {
            const line_col = utils.indexToLineCol(input, ast_item.range.start);

            std.debug.print(
                "{}: error: an item called “{s}” has already been defined\n",
                .{ line_col, ast_item.name },
            );
            std.os.exit(92);
        }

        try items.put(ast_item.name, item);
    }

    return .{ .items = items };
}

pub const FileIndex = struct {
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
        strukt: Struct,
    };

    pub const Function = struct {};

    pub const Struct = struct {
        fields: std.StringHashMap(Field),

        pub const Field = struct {
            ty: Type,
        };
    };
};

pub const Type = struct {
    name: []const u8,
};

fn lowerStruct(strukt: Ast.Item.Struct, input: []const u8, a: Allocator) !Item.Data {
    var fields = std.StringHashMap(Item.Struct.Field).init(a);

    for (strukt.fields.items) |ast_field| {
        if (fields.contains(ast_field.name)) {
            const line_col = utils.indexToLineCol(input, ast_field.range.start);

            std.debug.print(
                "{}: error: a field called “{s}” has already been defined on this struct\n",
                .{ line_col, ast_field.name },
            );
            std.os.exit(92);
        }

        const field = .{ .ty = lowerType(ast_field.ty) };
        try fields.put(ast_field.name, field);
    }

    return .{
        .strukt = .{
            .fields = fields,
        },
    };
}

fn lowerType(ty: Ast.Type) Type {
    return .{ .name = ty.name };
}

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
            .strukt => |strukt| self.printStrukt(name, strukt),
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

    fn printStrukt(
        self: *PrettyPrintContext,
        name: []const u8,
        strukt: Item.Struct,
    ) Error!void {
        try self.writer.print("struct {s} {{", .{name});
        var iterator = strukt.fields.iterator();
        while (iterator.next()) |entry| {
            const field_name = entry.key_ptr.*;
            const field = entry.value_ptr.*;

            try self.writer.print("\n\t{s} ", .{field_name});
            try self.printType(field.ty);
            try self.writer.writeByte(',');
        }
        try self.writer.writeAll("\n}");
    }

    fn printType(self: *PrettyPrintContext, ty: Type) Error!void {
        try self.writer.writeAll(ty.name);
    }
};
