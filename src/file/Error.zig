const std = @import("std");
const Loc = @import("./Loc.zig");
const WgslParsingError = @import("../wgsl/ParsingError.zig");
const WgslToken = @import("../wgsl/Token.zig");

const Self = @This();
const Config = std.io.tty.Config;

severity: Severity = .@"error",
src_offset: Loc.Index,
data: Data,

pub const Severity = enum {
    @"error",
    warning,
    note,
};

pub const Data = union(enum) {
    wgsl: struct {
        tag: WgslParsingError.Tag,
        expected_token: WgslToken.Tag = .invalid,
    },
    unresolved_module: struct {
        errname: []const u8,
        mod_path: []const u8,
    },
    symbol_already_declared: struct {
        //other_loc: *const Self,
    },
};

pub const ErrorLoc = struct {
    line_num: Loc.Index,
    line_start: Loc.Index,
    tok_start: Loc.Index,
    tok_end: Loc.Index,
};

fn writeHeader(
    writer: anytype,
    term: Config,
    path: ?[]const u8,
    line_num: Loc.Index,
    col_num: Loc.Index,
) !void {
    // 'file:line:column error: MSG'
    try term.setColor(writer, .bold);
    if (path) |f| try writer.print("{s}:", .{f});
    try writer.print("{d}:{d}\n", .{ line_num, col_num + 1 });
    try term.setColor(writer, .reset);
    try term.setColor(writer, .dim);
    try writer.print("{d} │ ", .{line_num});
    try term.setColor(writer, .reset);
}

fn writeSource(
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

fn writePointer(
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

pub fn write(
    self: Self,
    writer: anytype,
    term: std.io.tty.Config,
    path: ?[]const u8,
    source: [:0]const u8,
    loc: ErrorLoc,
) !void {
    const col_num = loc.tok_start - loc.line_start;
    try writeHeader(writer, term, path, loc.line_num, col_num);
    try writeSource(writer, term, source, loc.line_start, loc.tok_start, loc.tok_end);

    // See renderHeader
    const line_number_len = std.math.log10(loc.line_num) + 4;
    try writePointer(writer, term, col_num + line_number_len);

    switch (self.data) {
        .wgsl => |e| {
            try e.tag.render(writer, e.expected_token);
        },
        .unresolved_module => |e| {
            try writer.print("error: {s} when opening file {s}", .{ e.errname, e.mod_path });
        },
        .symbol_already_declared => {
            try writer.writeAll("error: symbol already declared");
            //try s.other_loc.write(writer, term);
        },
    }
    try term.setColor(writer, .reset);
    try writer.writeByte('\n');
}
