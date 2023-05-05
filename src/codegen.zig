const std = @import("std");
const Allocator = std.mem.Allocator;
const Lir = @import("Lir.zig");

pub fn codegen(input: []const u8, lir: Lir, a: Allocator) ![]const u8 {
    var assembly = std.ArrayList(u8).init(a);

    try assembly.appendSlice(".intel_syntax noprefix\n");

    var iterator = lir.bodies.iterator();
    while (iterator.next()) |entry| {
        const name = entry.key_ptr.*;
        const body = entry.value_ptr.*;

        var local_offsets = try std.ArrayList(u32)
            .initCapacity(a, body.local_types.items.len);

        var current_offset: u32 = 0;
        for (body.local_types.items) |ty| {
            current_offset += ty.size();
            try local_offsets.append(current_offset);
        }

        var context = CodegenContext{
            .assembly = &assembly,
            .input = input,
            .body = body,
            .depth = 0,
            .function_name = name,
            .local_offsets = local_offsets.items,
        };

        try context.genFunction();
    }

    return assembly.items;
}

const CodegenContext = struct {
    assembly: *std.ArrayList(u8),
    input: []const u8,
    body: Lir.Body,
    depth: u32,
    function_name: []const u8,
    local_offsets: []const u32,

    fn genFunction(self: *CodegenContext) !void {
        try self.print(".global _{s}\n", .{self.function_name});
        try self.print("_{s}:\n", .{self.function_name});

        for (self.body.instructions.items) |instruction| {
            try self.genInstruction(instruction);
        }

        try self.print(".L.return.{s}:\n", .{self.function_name});
        try self.pop("rax");
        try self.print("\tret\n", .{});

        std.debug.assert(self.depth == 0);
    }

    fn genInstruction(self: *CodegenContext, instruction: Lir.Instruction) !void {
        switch (instruction.data) {
            .push => |value| {
                try self.print("\tmov\trax, {}\n", .{value});
                try self.push();
            },

            .lst => |local_index| {
                try self.print("\tlea\trdi, [rbp + {}]\n", .{self.local_offsets[local_index]});
                const ty = self.body.local_types.items[local_index];
                try self.store(ty);
            },

            .ret => try self.print("\tjmp\t.L.return.{s}\n", .{self.function_name}),

            .add,
            .sub,
            .mul,
            .div,
            .mod,
            .or_,
            .and_,
            .xor,
            .shl,
            .shr,
            .lt,
            .le,
            .gt,
            .ge,
            .eq,
            .ne,
            => try self.genBinary(instruction),
        }
    }

    fn genBinary(self: *CodegenContext, instruction: Lir.Instruction) !void {
        try self.pop("rdi"); // rhs
        try self.pop("rax"); // lhs

        switch (instruction.data) {
            .add => try self.print("\tadd\trax, rdi\n", .{}),
            .sub => try self.print("\tsub\trax, rdi\n", .{}),
            .mul => try self.print("\timul\trax, rdi\n", .{}),

            .div => {
                try self.print("\tcqo\n", .{});
                try self.print("\tidiv\trdi\n", .{});
            },
            .mod => {
                try self.print("\tcqo\n", .{});
                try self.print("\tidiv\trdi\n", .{});
                try self.print("\tmov\trax, rdx\n", .{});
            },

            .or_ => try self.print("\tor\trax, rdi\n", .{}),
            .and_ => try self.print("\tand\trax, rdi\n", .{}),
            .xor => try self.print("\txor\trax, rdi\n", .{}),

            .shl => {
                try self.print("\tmov\trcx, rdi\n", .{});
                try self.print("\tshl\trax, cl\n", .{});
            },
            .shr => {
                try self.print("\tmov\trcx, rdi\n", .{});
                try self.print("\tsar\trax, cl\n", .{});
            },

            .lt, .le, .gt, .ge, .eq, .ne => {
                try self.print("\tcmp\trax, rdi\n", .{});
                switch (instruction.data) {
                    .lt => try self.print("\tsetl\tal\n", .{}),
                    .le => try self.print("\tsetle\tal\n", .{}),
                    .gt => try self.print("\tsetg\tal\n", .{}),
                    .ge => try self.print("\tsetge\tal\n", .{}),
                    .eq => try self.print("\tsete\tal\n", .{}),
                    .ne => try self.print("\tsetne\tal\n", .{}),
                    else => unreachable,
                }
                try self.print("\tmovzx\trax, al\n", .{});
            },

            else => unreachable,
        }

        try self.push();
    }

    fn store(self: *CodegenContext, ty: Lir.Type) !void {
        try self.pop("rax");
        switch (ty) {
            .i32 => try self.print("\tmov\t[rdi], rax\n", .{}),
        }
    }

    fn push(self: *CodegenContext) !void {
        try self.print("\tpush\trax\n", .{});
        self.depth += 1;
    }

    fn pop(self: *CodegenContext, register: []const u8) !void {
        try self.print("\tpop\t{s}\n", .{register});
        self.depth -= 1;
    }

    fn print(
        self: *CodegenContext,
        comptime fmt: []const u8,
        args: anytype,
    ) !void {
        try self.assembly.writer().print(fmt, args);
    }
};