const std = @import("std");

const Self = @This();

const PEEK_SIZE = 25;
const WORD_LIMIT = 30; // keep this relitvely big because we want to only print a slice of the line if it has a lot of words in it
const CHARACTER_LIMIT = 256;

orginal_buffer: []const u8,
start: usize = 0,
end: usize = 0,
line: u32 = 0,
column: u32 = 0,
multi_line: bool = false,

pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    // // print orginal buffer + start - PEEK_SIZE unless it is negative
    // // print orginal buffer + end + PEEK_SIZE unless it is greater than the length of the buffer or end of line
    // // with squiggle lines under start + end
    // const start = if (self.start <= PEEK_SIZE) 0 else self.start - PEEK_SIZE;
    // const end = if (self.end + PEEK_SIZE >= self.orginal_buffer.len) self.orginal_buffer.len else self.end + PEEK_SIZE;
    //
    // // get how much space we need to get squiggle lines
    // const width = self.end - self.start;
    // const offset = self.start - start;
    //
    // try writer.print("{s}\n", .{self.orginal_buffer[start..end]});
    // try writer.writeByteNTimes(' ', offset);
    // try writer.writeByteNTimes('^', width);
    // PLEASE NOTE LINE CAN BE MULTIPLE LINES
    // first step: find the start of the line need
    const start_line = std.mem.lastIndexOfScalar(u8, self.orginal_buffer[0..self.start], '\n') orelse 0;

    // second step: find the end of the line need
    const end_line = std.mem.indexOfScalar(u8, self.orginal_buffer[self.end..], '\n') orelse self.orginal_buffer.len;
    const line = self.orginal_buffer[start_line..end_line];

    // third step: find how many words are in the line
    var words = std.mem.tokenizeScalar(u8, line, ' '); // this is the best way i found to do this because we don't wanna count every space
    var word_count: u32 = 0;
    while (words.next()) |_| {
        word_count += 1;
    }

    if (word_count > WORD_LIMIT or line.len > CHARACTER_LIMIT) {
        const start = if (self.start <= PEEK_SIZE) 0 else self.start - PEEK_SIZE;
        const end = if (self.end + PEEK_SIZE >= self.orginal_buffer.len) self.orginal_buffer.len else self.end + PEEK_SIZE;

        const cutoff_start = blk: {
            break :blk self.orginal_buffer[if (self.start - 1 < 0) break :blk false else self.start - 1] == ' ';
        };
        const cutoff_end = blk: {
            break :blk self.orginal_buffer[if (self.end + 1 >= self.orginal_buffer.len) break :blk false else self.end + 1] == ' ';
        };

        // make sure to ignore the spaces
        const new_start = if (cutoff_start) if (std.mem.lastIndexOfScalar(u8, self.orginal_buffer[0..start], ' ')) |index| index + 1 else 0 else start;
        const new_end = if (cutoff_end) if (std.mem.indexOfScalar(u8, self.orginal_buffer[end..], ' ')) |index| index - 1 else self.orginal_buffer.len else end;

        const text = self.orginal_buffer[new_start..new_end];

        const width = self.end - self.start;
        const offset = self.start - new_start;
        const missing_start = new_start > start_line; // if the start is missing then we need to print the ...
        const missing_end = new_end < end_line;

        const missing_start_string = "... ";
        if (missing_start) try writer.print(missing_start_string, .{});
        try writer.print("{s}", .{text});
        if (missing_end) try writer.print(" ...", .{});
        try writer.writeByte('\n');
        const multi_line = std.mem.containsAtLeastScalar(u8, text, 1, '\n');
        if (!multi_line) {
            try writer.writeByteNTimes(' ', offset + if (missing_start) missing_start_string.len else 0);
            try writer.writeByteNTimes('^', width);
        }
    } else {
        const width = self.end - self.start;
        const offset = self.start - start_line;

        try writer.print("{s}\n", .{line});
        if (!self.multi_line) {
            try writer.writeByteNTimes(' ', offset);
            try writer.writeByteNTimes('^', width);
        }
    }
}

pub fn combine(self: Self, other: Self) Self {
    var multi_line = self.multi_line or other.multi_line;
    multi_line = multi_line or self.line != other.line;
    return Self{
        .orginal_buffer = self.orginal_buffer,
        .start = self.start,
        .end = other.end,
        .line = self.line,
        .column = self.column,
        .multi_line = multi_line,
    };
}
