const std = @import("std");
const builtin = @import("builtin");
const Lir = @import("Lir.zig");
const Allocator = std.mem.Allocator;
const ALU = std.ArrayListUnmanaged;
const SAHMU = std.StringArrayHashMapUnmanaged;

pub fn codegen(source: []const u8, lir: Lir, a: Allocator) ![]const u8 {
    var assembly = ALU(u8){};

    try assembly.appendSlice(a, ".intel_syntax noprefix\n");

    for (lir.bodies.keys(), lir.bodies.values()) |name, *body| {
        var local_offsets = try ALU(u32).initCapacity(a, body.local_types.items.len);
        defer local_offsets.deinit(a);

        var current_offset: u32 = 0;
        for (body.local_types.items) |ty| {
            current_offset += current_offset % ty.alignment();
            current_offset += ty.size();
            local_offsets.appendAssumeCapacity(current_offset);
        }

        const align_bits = 4;
        const stack_size = ((current_offset + (1 << align_bits) - 1) >> align_bits) << align_bits;

        var context = CodegenContext{
            .assembly = &assembly,
            .allocator = a,
            .source = source,
            .body = body,
            .function_name = name,
            .local_offsets = local_offsets.items,
            .stack_size = stack_size,
            .bodies = &lir.bodies,
        };

        try context.genFunction();
    }

    return assembly.toOwnedSlice(a);
}

const param_registers = [_][]const u8{ "rdi", "rsi", "rdx", "rcx", "r8", "r9" };

const CodegenContext = struct {
    const Self = @This();

    assembly: *ALU(u8),
    allocator: Allocator,
    source: []const u8,
    body: *const Lir.Body,
    function_name: []const u8,
    local_offsets: []const u32,
    stack_size: u32,
    bodies: *const SAHMU(Lir.Body),
    push_queued: bool = false,

    fn genFunction(self: *Self) !void {
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

        for (self.body.params, 0..) |ty, i| {
            try self.print("\tmov\trax, {s}\n", .{param_registers[i]});
            try self.print("\tmov\t[rbp - {}], {s}\n", .{ self.local_offsets[i], getSizedRax(ty) });
        }

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

    fn genInstruction(self: *Self, instruction: Lir.Instruction) !void {
        const writer = try self.assemblyWriter();
        try writer.print("\t# {}\n", .{instruction});

        switch (instruction.data) {
            .push => |value| {
                try self.print("\tmov\t{s}, {}\n", .{ getSizedRax(instruction.ty), value });
                try self.push();
            },

            .drop => try self.popRAX(),

            .laddr => |local_index| {
                try self.genLocalAddress(local_index);
            },

            .b => |label| {
                try self.print("\tjmp\t.L.{}\n", .{label.index});
            },

            .cbz => |label| {
                try self.popRAX();
                try self.print("\tcmp\trax, 0\n", .{});
                try self.print("\tje\t.L.{}\n", .{label.index});
            },

            .ld => try self.load(instruction.ty),
            .st_vp => try self.store_vp(instruction.ty),
            .st_pv => try self.store_pv(instruction.ty),

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

            .call => |func_name| try self.genCall(func_name),
        }
    }

    fn genCall(self: *Self, func_name: []const u8) !void {
        _ = func_name;
        _ = self;
        unreachable;
    }

    fn genCast(self: *Self, src_type: Lir.IRType, dest_type: Lir.IRType) !void {
        try self.popRAX();
        if (src_type.isSigned()) {
            try self.print("\tmovsx\t{s}, {s}\n", .{ getSizedRax(dest_type), getSizedRax(src_type) });
        } else {
            try self.print("\tmovzx\t{s}, {s}\n", .{ getSizedRax(dest_type), getSizedRax(src_type) });
        }
        try self.push();
    }

    fn genBinary(self: *Self, instruction: Lir.Instruction) !void {
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
                    else => unreachable,
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

    fn load(self: *Self, ty: Lir.Type) !void {
        try self.popRAX();
        try self.print("\tmov\t{s}, [rax]\n", .{getSizedRax(ty)});
        try self.push();
    }

    fn store_pv(self: *Self, ty: Lir.Type) !void {
        const value_reg = getSizedRdi(ty);
        // try self.pop(value_reg); // pop value to store
        try self.pop("rdi"); // pop value to store
        try self.popRAX(); // pop destination address
        try self.print("\tmov\t[rax], {s}\n", .{value_reg});
    }

    fn store_vp(self: *Self, ty: Lir.Type) !void {
        try self.popRAX(); // pop destination address
        try self.pop("rdi"); // pop value to store
        const value_reg = getSizedRdi(ty);
        // try self.pop(value_reg); // pop value to store
        try self.print("\tmov\t[rax], {s}\n", .{value_reg});
    }

    fn getSizedRax(ty: Lir.Type) []const u8 {
        return switch (ty) {
            .i32, .u32 => "eax",
            .i64, .u64 => "rax",
            .u8 => "al",
            .pointer => "rax",
            .void => unreachable,
        };
    }

    fn getSizedRdi(ty: Lir.Type) []const u8 {
        return switch (ty) {
            .i32, .u32 => "edi",
            .i64, .u64 => "rdi",
            .u8 => "dil",
            .pointer => "rdi",
            .void => unreachable,
        };
    }

    fn genLocalAddress(self: *Self, local_index: u32) !void {
        const offset = self.local_offsets[local_index];
        try self.print("\tlea\trax, [rbp - {}]\n", .{offset});
        try self.push();
    }

    fn push(self: *Self) !void {
        std.debug.assert(!self.push_queued);
        self.push_queued = true;
    }

    fn popRAX(self: *Self) !void {
        if (self.push_queued) {
            self.push_queued = false;
            return;
        }
        try self.print("\tpop\trax\n", .{});
    }

    fn pop(self: *Self, register: []const u8) !void {
        if (self.push_queued) {
            self.push_queued = false;
            // try self.assembly.writer().writeAll("\tpush\trax\n");
            try self.print("\tmov\t{s}, rax\n", .{register});
        } else {
            try self.print("\tpop\t{s}\n", .{register});
        }
    }

    fn print(
        self: *Self,
        comptime fmt: []const u8,
        args: anytype,
    ) !void {
        var writer = try self.assemblyWriter();
        if (self.push_queued) {
            self.push_queued = false;
            try writer.writeAll("\tpush\trax\n");
        }
        try writer.print(fmt, args);
    }

    fn assemblyWriter(self: *Self) !ALU(u8).Writer {
        return self.assembly.writer(self.allocator);
    }
};
