// Maximum file size
pub const Index = u32;

start: Index,
end: Index,

const Self = @This();

pub const ExtraInfo = struct {
    line: u32,
    col: u32,
    line_start: u32,
    line_end: u32,
};

pub fn extraInfo(self: Self, source: []const u8) ExtraInfo {
    var result = ExtraInfo{
        .line = 1,
        .col = 1,
        .line_start = 0,
        .line_end = @intCast(source.len),
    };

    for (source[0..self.start], 0..) |c, i| {
        if (c == '\n') {
            result.line += 1;
            result.line_start = @as(u32, @intCast(i)) + 1;
        }
    }

    for (source[self.end..], 0..) |c, i| {
        if (c == '\n') {
            result.line_end = self.end + @as(u32, @intCast(i));
            break;
        }
    }

    result.col += self.start - result.line_start;
    return result;
}
