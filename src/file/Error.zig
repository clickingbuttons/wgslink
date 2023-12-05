const std = @import("std");
const Loc = @import("./Loc.zig");
const WgslParsingError = @import("../wgsl/ParsingError.zig");
const WgslToken = @import("../wgsl/Token.zig");
const AliaserErrorTag = @import("../ast/Aliaser.zig").ErrorTag;

const Self = @This();
const Config = std.io.tty.Config;
pub const ErrorLoc = struct {
    line_num: Loc.Index,
    line_start: Loc.Index,
    tok_start: Loc.Index,
    tok_end: Loc.Index,

    pub fn init(newlines: []Loc.Index, tok_start: Loc.Index, tok_end: Loc.Index) @This() {
        var i: usize = 0;
        var line_num: Loc.Index = 1;
        var line_start: Loc.Index = 0;
        while (tok_start >= newlines[i]) : (i += 1) {
            line_num += 1;
            line_start = newlines[i] + 1;
        }
        return .{
            .line_num = line_num,
            .line_start = line_start,
            .tok_start = tok_start,
            .tok_end = tok_end,
        };
    }
};

severity: Severity = .@"error",
path: ?[]const u8,
source: [:0]const u8,
loc: ErrorLoc,
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
    aliasing: struct {
        tag: AliaserErrorTag,
    },
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
    sev: Severity,
) !void {
    try writer.writeByteNTimes(' ', col_num);
    try term.setColor(writer, .bold);
    const color: std.io.tty.Color = switch (sev) {
        .@"error" => .red,
        .warning => .yellow,
        .note => .blue,
    };
    try term.setColor(writer, color);
    try writer.writeByte('^');
    try writer.writeByte(' ');
    try writer.writeAll(@tagName(sev));
    try writer.writeAll(": ");
}

pub fn write(self: Self, writer: anytype, term: std.io.tty.Config) !void {
    const loc = self.loc;
    const path = self.path;
    const source = self.source;
    const col_num = loc.tok_start - loc.line_start;
    try writeHeader(writer, term, path, loc.line_num, col_num);
    try writeSource(writer, term, source, loc.line_start, loc.tok_start, loc.tok_end);

    // See renderHeader
    const line_number_len = std.math.log10(loc.line_num) + 4;
    try writePointer(writer, term, col_num + line_number_len, self.severity);

    switch (self.data) {
        .wgsl => |e| {
            try e.tag.render(writer, e.expected_token);
        },
        .unresolved_module => |e| {
            try writer.print("{s} when opening file {s}", .{ e.errname, e.mod_path });
        },
        .aliasing => |a| switch (a.tag) {
            .symbol_already_declared => {
                const msg = switch (self.severity) {
                    .note => "symbol declared here",
                    else => "symbol already declared",
                };
                try writer.writeAll(msg);
            },
            .no_matching_export => {
                try writer.writeAll("no matching export");
            },
        },
    }
    try term.setColor(writer, .reset);
    try writer.writeByte('\n');
}
