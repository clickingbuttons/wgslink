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
        extra: Loc.Index = 0,
    },
    unresolved_module: struct {
        errname: []const u8,
        mod_path: []const u8,
    },
    aliasing: struct {
        tag: AliaserErrorTag,
    },
};

fn writeHeader(self: Self, writer: anytype, term: Config) !void {
    const line_num = self.loc.line_num;
    const col_num = self.loc.tok_start - self.loc.line_start;
    // 'file:line:column error: MSG'
    try term.setColor(writer, .bold);
    if (self.path) |f| try writer.print("{s}:", .{f});
    try writer.print("{d}:{d}\n", .{ line_num, col_num + 1 });
    try term.setColor(writer, .reset);
    try term.setColor(writer, .dim);
    try writer.print("{d} │ ", .{line_num});
    try term.setColor(writer, .reset);
}

fn writeSource(self: Self, writer: anytype, term: Config) !void {
    const source = self.source;
    const tok_start = self.loc.tok_start;
    const tok_end = self.loc.tok_end;

    try writer.writeAll(source[self.loc.line_start..tok_start]);
    try term.setColor(writer, .green);
    try writer.writeAll(source[tok_start..tok_end]);
    try term.setColor(writer, .reset);
    var i: Loc.Index = tok_end;
    while (source[i] != '\n') : (i += 1) try writer.writeByte(source[i]);
    try writer.writeByte('\n');
}

fn writePointer(self: Self, writer: anytype, term: Config) !void {
    // See renderHeader
    const line_number_len = std.math.log10(self.loc.line_num) + 4;
    try writer.writeByteNTimes(' ', line_number_len);
    for (self.source[self.loc.line_start..self.loc.tok_start]) |c| switch (c) {
        '\t' => try writer.writeByte(c),
        else => try writer.writeByte(' '),
    };
    try term.setColor(writer, .bold);
    const color: std.io.tty.Color = switch (self.severity) {
        .@"error" => .red,
        .warning => .yellow,
        .note => .blue,
    };
    try term.setColor(writer, color);
    try writer.writeByte('^');
    try writer.writeByte(' ');
    try writer.writeAll(@tagName(self.severity));
    try writer.writeAll(": ");
}

pub fn write(self: Self, writer: anytype, term: std.io.tty.Config) !void {
    try self.writeHeader(writer, term);
    try self.writeSource(writer, term);
    try self.writePointer(writer, term);

    switch (self.data) {
        .wgsl => |e| {
            try e.tag.render(writer, e.extra);
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
            .unresolved_ref => {
                try writer.writeAll("unresolved reference will not be minified");
            },
        },
    }
    try term.setColor(writer, .reset);
    try writer.writeByte('\n');
}
