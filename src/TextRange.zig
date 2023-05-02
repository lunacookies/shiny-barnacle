const std = @import("std");
const TextRange = @This();

start: u32,
end: u32,

pub fn format(
    self: TextRange,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;
    try writer.print("{d}..{d}", .{ self.start, self.end });
}
