const std = @import("std");

const Self = @This();

const PEEK_SIZE = 25; // number of characters to peek

orginal_buffer: []const u8,
start: usize = 0,
end: usize = 0,
line: u32 = 0,
column: u32 = 0,

pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    // print orginal buffer + start - PEEK_SIZE unless it is negative
    // print orginal buffer + end + PEEK_SIZE unless it is greater than the length of the buffer or end of line
    // with squiggle lines under start + end
    const start = if (self.start <= PEEK_SIZE) 0 else self.start - PEEK_SIZE;
    const end = if (self.end + PEEK_SIZE >= self.orginal_buffer.len) self.orginal_buffer.len else self.end + PEEK_SIZE;

    // get how much space we need to get squiggle lines
    const width = self.end - self.start;
    const offset = self.start - start;

    try writer.print("{s}\n", .{self.orginal_buffer[start..end]});
    try writer.writeByteNTimes(' ', offset);
    try writer.writeByteNTimes('^', width);
}

pub fn combine(self: Self, other: Self) Self {
    return Self{
        .orginal_buffer = self.orginal_buffer,
        .start = self.start,
        .end = other.end,
        .line = self.line,
        .column = self.column,
    };
}
