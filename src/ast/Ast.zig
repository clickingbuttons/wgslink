/// Immutable abstract syntax tree.
const std = @import("std");
const Node = @import("./Node.zig");
const WgslParsingError = @import("../wgsl/ParsingError.zig");
const WgslTokenizer = @import("../wgsl/Tokenizer.zig");
const Language = @import("../file/File.zig").Language;
const FileError = @import("../file/Error.zig");
const File = @import("../file/File.zig");

const Allocator = std.mem.Allocator;
const Self = @This();
const Loc = File.Loc;
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
/// Offsets of newlines for error messages
newlines: []Loc.Index,
/// For locating language dependent errors
from_lang: Language,
/// No one's perfect...
errors: []File.Error,

pub fn deinit(self: *Self, allocator: Allocator) void {
    self.nodes.deinit(allocator);
    for (self.identifiers.items(.key)) |k| allocator.free(k);
    self.identifiers.deinit(allocator);
    allocator.free(self.extra);
    allocator.free(self.newlines);
    allocator.free(self.errors);
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

pub fn getErrorLoc(
    self: Self,
    source: [:0]const u8,
    src_offset: Loc.Index,
    token: anytype,
) FileError.ErrorLoc {
    var tok_start: Loc.Index = 0;
    var tok_end: Loc.Index = 0;
    switch (self.from_lang) {
        .wgsl => {
            var tokenizer = WgslTokenizer.init(source[src_offset..]);
            while (true) {
                const tok = tokenizer.next();
                if (tok.tag == token or tok.tag == .eof) {
                    tok_start = tok.loc.start;
                    tok_end = tok.loc.end;
                    break;
                }
            }
        },
    }

    return FileError.ErrorLoc.init(self.newlines, src_offset + tok_start, src_offset + tok_end);
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
    std.debug.assert(n.tag == .span);
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
