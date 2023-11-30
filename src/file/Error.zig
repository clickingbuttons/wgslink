const std = @import("std");
const Loc = @import("./Loc.zig");

pub const Tag = enum {
    unresolved_module,
};

pub const ErrorLoc = struct {
    line_num: Loc.Index,
    line_start: Loc.Index,
    tok_start: Loc.Index,
    tok_end: Loc.Index,
};

pub fn renderHeader(
    writer: anytype,
    term: std.io.tty.Config,
    file_path: ?[]const u8,
    line_num: Loc.Index,
    col_num: Loc.Index,
) !void {
    // 'file:line:column error: MSG'
    try term.setColor(writer, .bold);
    if (file_path) |f| try writer.print("{s}:", .{f});
    try writer.print("{d}:{d}\n", .{ line_num, col_num + 1 });
    try term.setColor(writer, .reset);
    try term.setColor(writer, .dim);
    try writer.print("{d} │ ", .{line_num});
    try term.setColor(writer, .reset);
}

pub fn renderSource(
    writer: anytype,
    term: std.io.tty.Config,
    source: []const u8,
    line_start: Loc.Index,
    tok_start: Loc.Index,
    tok_end: Loc.Index,
) !void {
    try writer.writeAll(source[line_start..tok_start]);
    try term.setColor(writer, .green);
    try writer.writeAll(source[tok_start..tok_end]);
    try term.setColor(writer, .reset);
    var i: Loc.Index = tok_end;
    while (source[i] != '\n') : (i += 1) try writer.writeByte(source[i]);
    try writer.writeByte('\n');
}

pub fn renderPointer(
    writer: anytype,
    term: std.io.tty.Config,
    col_num: Loc.Index,
) !void {
    try writer.writeByteNTimes(' ', col_num);
    try term.setColor(writer, .bold);
    try term.setColor(writer, .red);
    try writer.writeByte('^');
    try writer.writeByte(' ');
}

pub fn render(
    writer: anytype,
    term: std.io.tty.Config,
    source: []const u8,
    file_path: ?[]const u8,
    loc: ErrorLoc,
) !void {
    const col_num = loc.tok_start - loc.line_start;
    try renderHeader(writer, term, file_path, loc.line_num, col_num);
    try renderSource(writer, term, source, loc.line_start, loc.tok_start, loc.tok_end);

    // See renderHeader
    const line_number_len = std.math.log10(loc.line_num) + 4;
    try renderPointer(writer, term, col_num + line_number_len);
}
