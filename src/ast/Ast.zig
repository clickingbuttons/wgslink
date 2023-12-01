/// Immutable abstract syntax tree.
const std = @import("std");
const Node = @import("./Node.zig");
const WgslParsingError = @import("../wgsl/ParsingError.zig");
const WgslTokenizer = @import("../wgsl/Tokenizer.zig");
const Language = @import("../file/File.zig").Language;
const FileError = @import("../file/Error.zig");
const Loc = @import("../file/Loc.zig");

const Allocator = std.mem.Allocator;
const Self = @This();
pub const NodeList = std.MultiArrayList(Node);
pub const Identifiers = std.MultiArrayList(std.StringArrayHashMapUnmanaged(void).Data);

/// Node 0 is a span of directives followed by declarations. Since there can be no
/// references to this root node, 0 is available to indicate null
nodes: NodeList.Slice,
/// Nodes with identifers store indexes into here. They stored offset by +1 so that `0` is a null sentinel.
/// For `var foo: u32 = baz();` this will store `foo`, `u32`, and `baz`
/// Owns the strings so that `source` may be freed after parsing is finished.
identifiers: Identifiers.Slice,
/// For nodes with more data than @sizeOf(Node)
extra: []Node.Index,
/// For rendering lannguage-dependent errors
from_lang: Language,
/// Offsets of newlines for error messages
newlines: []Loc.Index,

pub fn deinit(self: *Self, allocator: Allocator) void {
    self.nodes.deinit(allocator);
    for (self.identifiers.items(.key)) |k| allocator.free(k);
    self.identifiers.deinit(allocator);
    allocator.free(self.extra);
    allocator.free(self.newlines);
    self.* = undefined;
}

pub fn identifier(self: Self, index: Node.IdentIndex) []const u8 {
    return self.identifiers.get(index - 1).key;
}

pub fn extraData(self: Self, comptime T: type, index: Node.Index) T {
    const fields: []const std.builtin.Type.StructField = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        @field(result, field.name) = self.extra[index + i];
    }
    return result;
}

pub fn node(self: Self, index: Node.Index) Node {
    return self.nodes.get(index);
}

pub fn spanToList(self: Self, index: ?Node.Index) []const Node.Index {
    if (index) |i| {
        std.debug.assert(self.node(i).tag == .span);
        const span = self.node(i);
        return self.extra[span.lhs..span.rhs];
    }
    return &.{};
}

pub fn hasError(self: Self) bool {
    for (self.spanToList(0)) |i| {
        switch (self.node(i).tag) {
            .@"error" => return true,
            else => {},
        }
    }
    return false;
}

pub fn getErrorLoc(self: Self, source: [:0]const u8, index: Node.Index) FileError.ErrorLoc {
    const n = self.node(index);
    var line_num: Loc.Index = 0;
    var line_start: Loc.Index = 0;

    var i: usize = 0;
    while (n.src_offset < self.newlines[i]) : (i += 1) {
        line_num += 1;
        line_start = self.newlines[i];
    }

    var tok_end = n.src_offset;
    switch (self.from_lang) {
        .wgsl => {
            var tokenizer = WgslTokenizer.init(source[n.src_offset..]);
            const tok = tokenizer.next();
            tok_end = tok.loc.end;
        },
    }

    return .{
        .line_num = line_num,
        .line_start = line_start,
        .tok_start = n.src_offset,
        .tok_end = tok_end,
    };
}

pub fn renderErrors(
    self: Self,
    writer: anytype,
    term: std.io.tty.Config,
    source: [:0]const u8,
    file_path: ?[]const u8,
) !void {
    for (self.spanToList(0)) |i| {
        const n = self.node(i);
        switch (n.tag) {
            .@"error" => {
                const error_loc = self.getErrorLoc(source, i);
                try FileError.render(writer, term, source, file_path, error_loc);
                switch (self.from_lang) {
                    .wgsl => {
                        const tag: WgslParsingError.Tag = @enumFromInt(n.lhs);
                        try tag.render(writer, @enumFromInt(n.rhs));
                    },
                }
                try term.setColor(writer, .reset);
                try writer.writeByte('\n');
            },
            else => {},
        }
    }
}

pub fn globalName(self: Self, index: Node.Index) []const u8 {
    const n = self.node(index);
    switch (n.tag) {
        .global_var => {
            const global_var = self.extraData(Node.GlobalVar, n.lhs);
            return self.identifier(global_var.name);
        },
        .override => {
            const override = self.extraData(Node.Override, n.lhs);
            return self.identifier(override.name);
        },
        .@"fn" => {
            const header = self.extraData(Node.FnHeader, n.lhs);
            return self.identifier(header.name);
        },
        .@"const" => {
            const typed_ident = self.extraData(Node.TypedIdent, n.lhs);
            return self.identifier(typed_ident.name);
        },
        .type_alias, .@"struct" => return self.identifier(n.lhs),
        else => return "",
    }
}

pub fn removeFromSpan(self: *Self, span_index: Node.Index, item_index: usize) void {
    const n = self.node(span_index);
    for (n.lhs + item_index..n.rhs - 1) |j| {
        self.extra[j] = self.extra[j + 1];
    }
    self.nodes.items(.rhs)[span_index] -= 1;
}

pub fn modName(self: Self, imp: Node.Index) []const u8 {
    const n = self.node(imp);
    return self.identifier(n.rhs);
}

pub fn modAliases(self: Self, imp: Node.Index) []const Node.Index {
    const n = self.node(imp);
    std.debug.assert(n.tag == .import);
    return self.spanToList(if (n.lhs == 0) null else n.lhs);
}
