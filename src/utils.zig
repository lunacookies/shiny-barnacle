const std = @import("std");

const LineCol = struct { line: u32, column: u32 };

pub fn indexToLineCol(text: []const u8, index: usize) LineCol {
    var line: u32 = 0;
    var column: u32 = 0;

    for (text, 0..) |c, i| {
        if (i == index) break;

        if (c == '\n') {
            line += 1;
            column = 0;
            continue;
        }

        column += 1;
    }

    return .{ .line = line, .column = column };
}
