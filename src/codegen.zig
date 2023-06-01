const std = @import("std");
const builtin = @import("builtin");
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
            current_offset += current_offset % ty.alignment();
            current_offset += ty.size();
            try local_offsets.append(current_offset);
        }

        var context = CodegenContext{
            .assembly = &assembly,
            .input = input,
            .body = body,
            .function_name = name,
            .local_offsets = local_offsets.items,
            .stack_size = current_offset,
        };

        try context.genFunction();
    }

    return assembly.items;
}

const CodegenContext = struct {
    assembly: *std.ArrayList(u8),
    input: []const u8,
    body: Lir.Body,
    function_name: []const u8,
    local_offsets: []const u32,
    stack_size: u32,
    push_queued: bool = false,

    fn genFunction(self: *CodegenContext) !void {
        switch (builtin.os.tag) {
            .macos => {
                try self.print(".global _{s}\n", .{self.function_name});
                try self.print("_{s}:\n", .{self.function_name});
            },
            else => {
                try self.print(".global {s}\n", .{self.function_name});
                try self.print("{s}:\n", .{self.function_name});
            },
        }

        try self.print("\tpush\trbp\n", .{});
        try self.print("\tmov\trbp, rsp\n", .{});
        try self.print("\tsub\trsp, {}\n", .{self.stack_size});

        for (self.body.instructions.items, 0..) |instruction, instruction_index| {
            for (self.body.labels.items, 0..) |label, label_index| {
                if (label.instruction_index == instruction_index) {
                    try self.print(".L.{}:\n", .{label_index});
                }
            }

            try self.genInstruction(instruction);
        }

        for (self.body.labels.items, 0..) |label, label_index| {
            if (label.instruction_index == self.body.instructions.items.len) {
                try self.print(".L.{}:\n", .{label_index});
            }
        }

        try self.print(".L.return.{s}:\n", .{self.function_name});
        try self.popRAX();
        try self.print("\tleave\n", .{});
        try self.print("\tret\n", .{});
    }

    fn genInstruction(self: *CodegenContext, instruction: Lir.Instruction) !void {
        switch (instruction.data) {
            .push => |value| {
                try self.print("\tmov\t{s}, {}\n", .{ getSizedRax(instruction.ty), value });
                try self.push();
            },

            .lst => |local_index| {
                try self.storeLocal(instruction.ty, local_index);
            },

            .lld => |local_index| {
                try self.loadLocal(instruction.ty, local_index);
            },

            .b => |label| {
                try self.print("\tjmp\t.L.{}\n", .{label.index});
            },

            .cbz => |label| {
                try self.pop("rax");
                try self.print("\tcmp\trax, 0\n", .{});
                try self.print("\tje\t.L.{}\n", .{label.index});
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

            .cast => |src_ty| try self.genCast(src_ty, instruction.ty),
        }
    }

    fn genCast(self: *@This(), src_ty: Lir.IRType, dest_ty: Lir.IRType) !void {
        try self.popRAX();
        if (dest_ty.isSigned()) {
            try self.print("\tmovsx\t{s}, {s}\n", .{ getSizedRax(dest_ty), getSizedRax(src_ty) });
        } else {
            try self.print("\tmovzx\t{s}, {s}\n", .{ getSizedRax(dest_ty), getSizedRax(src_ty) });
        }
        try self.push();
    }

    fn genBinary(self: *CodegenContext, instruction: Lir.Instruction) !void {
        try self.pop("rdi"); // rhs
        try self.popRAX(); // lhs

        const ty = instruction.ty;

        var op: ?[]const u8 = null;
        switch (instruction.data) {
            .add => op = "add",
            .sub => op = "sub",
            .mul => op = if (ty.isSigned()) "imul" else "mul",

            .div, .mod => {
                switch (ty) {
                    .i32 => {
                        try self.print("\tcdq\n", .{});
                        try self.print("\tidiv\tedi\n", .{});
                    },
                    .i64 => {
                        try self.print("\tcqo\n", .{});
                        try self.print("\tidiv\trdi\n", .{});
                    },
                    .u32, .u8 => {
                        try self.print("\txor edx, edx\n", .{});
                        try self.print("\tdiv\tedi\n", .{});
                    },
                    .u64 => {
                        try self.print("\txor edx, edx\n", .{});
                        try self.print("\tdiv\trdi\n", .{});
                    },
                }
                if (instruction.data == .mod)
                    try self.print("\tmov\trax, rdx\n", .{});
            },

            .or_ => op = "or",
            .and_ => op = "and",
            .xor => op = "xor",
            .shl => {
                try self.print("\tmov\trcx, rdi\n", .{});
                try self.print("\tshl\t{s}, cl\n", .{getSizedRax(ty)});
            },
            .shr => {
                try self.print("\tmov\trcx, rdi\n", .{});
                try self.print("\tsar\t{s}, cl\n", .{getSizedRax(ty)});
            },

            .lt, .le, .gt, .ge, .eq, .ne => {
                try self.print("\tcmp\t{s}, {s}\n", .{
                    getSizedRax(ty),
                    getSizedRdi(ty),
                });
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

        if (op) |opr| {
            try self.print("\t{s}\t{s}, {s}\n", .{
                opr,
                getSizedRax(ty),
                getSizedRdi(ty),
            });
        }

        try self.push();
    }

    fn getSizedRax(ty: Lir.Type) []const u8 {
        return switch (ty) {
            .i32, .u32 => "eax",
            .i64, .u64 => "rax",
            .u8 => "al",
        };
    }

    fn getSizedRdi(ty: Lir.Type) []const u8 {
        return switch (ty) {
            .i32, .u32 => "edi",
            .i64, .u64 => "rdi",
            .u8 => "dil",
        };
    }

    fn storeLocal(self: *CodegenContext, ty: Lir.Type, local_index: u32) !void {
        try self.popRAX();
        const reg = getSizedRax(ty);
        try self.print("\tmov\t[rbp - {}], {s}\n", .{ self.local_offsets[local_index], reg });
    }

    fn loadLocal(self: *CodegenContext, ty: Lir.Type, local_index: u32) !void {
        const reg = getSizedRax(ty);
        try self.print("\tmov\t{s}, [rbp - {}]\n", .{ reg, self.local_offsets[local_index] });
        try self.push();
    }

    fn push(self: *CodegenContext) !void {
        std.debug.assert(!self.push_queued);
        self.push_queued = true;
    }

    fn popRAX(self: *CodegenContext) !void {
        self.depth -= 1;
        if (self.push_queued) {
            self.push_queued = false;
            return;
        }
        try self.print("\tpop\trax\n", .{});
    }

    fn pop(self: *CodegenContext, register: []const u8) !void {
        self.depth -= 1;
        if (self.push_queued) {
            self.push_queued = false;
            try self.assembly.writer().writeAll("\tpush\trax\n");
        }
        try self.print("\tpop\t{s}\n", .{register});
    }

    fn print(
        self: *CodegenContext,
        comptime fmt: []const u8,
        args: anytype,
    ) !void {
        if (self.push_queued) {
            self.push_queued = false;
            try self.assembly.writer().writeAll("\tpush\trax\n");
        }
        try self.assembly.writer().print(fmt, args);
    }
};
