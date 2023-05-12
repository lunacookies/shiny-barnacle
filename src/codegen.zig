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
            current_offset += current_offset % ty.alignment();
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
    push_queued: bool = false,

    fn genFunction(self: *CodegenContext) !void {
        if (std.mem.eql(u8, self.function_name, "main")) {
            try self.print(".global _main\n", .{});
            try self.print("_main:\n", .{});
        }

        try self.print(".global {s}\n", .{self.function_name});
        try self.print("{s}:\n", .{self.function_name});

        try self.print("\tpush\trbp\n", .{});
        try self.print("\tmov\trbp, rsp\n", .{});
        try self.print("\tsub\trsp, {}\n", .{self.local_offsets[self.local_offsets.len - 1]});

        for (self.body.instructions.items) |instruction| {
            try self.genInstruction(instruction);
        }

        try self.print(".L.return.{s}:\n", .{self.function_name});
        try self.pop("rax");
        try self.print("\tleave\n", .{});
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
                // const ty = self.body.local_types.items[local_index];
                try self.storeLocal(instruction.ty, local_index);
            },

            .lld => |local_index| {
                // const ty = self.body.local_types.items[local_index];
                try self.loadLocal(instruction.ty, local_index);
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
        try self.pop("rax");
        if (dest_ty.isSigned()) {
            try self.print("\tmovsx\t{s}, {s}\n", .{ getSizedRax(dest_ty), getSizedRax(src_ty) });
        } else {
            try self.print("\tmovzx\t{s}, {s}\n", .{ getSizedRax(dest_ty), getSizedRax(src_ty) });
        }
        try self.push();
    }

    fn genBinary(self: *CodegenContext, instruction: Lir.Instruction) !void {
        try self.pop("rdi"); // rhs
        try self.pop("rax"); // lhs

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
        try self.pop("rax");
        const reg = getSizedRax(ty);
        try self.print("\tmov\t[rbp - {}], {s}\n", .{ self.local_offsets[local_index], reg });
    }

    fn loadLocal(self: *CodegenContext, ty: Lir.Type, local_index: u32) !void {
        const reg = getSizedRax(ty);
        try self.print("\tmov\t{s}, [rbp - {}]\n", .{ reg, self.local_offsets[local_index] });
        try self.push();
    }

    fn push(self: *CodegenContext) !void {
        // try self.print("\tpush\trax\n", .{});
        self.push_queued = true;
        self.depth += 1;
    }

    fn pop(self: *CodegenContext, register: []const u8) !void {
        self.depth -= 1;
        if (self.push_queued and std.mem.eql(u8, register, "rax")) {
            self.push_queued = false;
            return;
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
