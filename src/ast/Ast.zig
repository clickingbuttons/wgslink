/// Immutable abstract syntax tree.
const std = @import("std");
const builtin = @import("builtin");
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
extra: []align(@alignOf(Node.Index)) u8,
/// Offsets of newlines for error messages
newlines: []Loc.Index,
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
    if (index == 0) return "";
    return self.identifiers.get(index - 1).key;
}

pub fn extraData(self: Self, comptime T: type, index: Node.ExtraIndex) T {
    // std.debug.print("read {d} size={d} {}\n", .{ index, @sizeOf(T), T });
    const bytes = self.extra[index..][0..@sizeOf(T)];
    return std.mem.bytesAsValue(T, bytes).*;
}

pub fn node(self: Self, index: Node.Index) Node {
    return self.nodes.get(index);
}

pub fn checkSpan(lhs: Node.Index, rhs: Node.Index) void {
    if (builtin.mode == .Debug) {
        const aligned = (rhs - lhs) % @sizeOf(Node.Index);
        if (aligned != 0) {
            std.debug.panic("bad span lhs={d} rhs={d}\n", .{ lhs, rhs });
        }
    }
}

pub fn spanToList(self: Self, index: ?Node.Index) []const Node.Index {
    if (index) |i| {
        const span = self.node(i);
        std.debug.assert(span.tag == .span);
        checkSpan(span.lhs, span.rhs);
        const bytes = self.extra[span.lhs..span.rhs];
        const unaligned = std.mem.bytesAsSlice(Node.Index, bytes);
        // Is properly aligned in `listToSpan`.
        return @alignCast(unaligned);
    }
    return &.{};
}

pub fn getErrorLoc(
    self: Self,
    lang: Language,
    source: [:0]const u8,
    src_offset: Loc.Index,
    token: anytype,
) FileError.ErrorLoc {
    var tok_start: Loc.Index = 0;
    var tok_end: Loc.Index = 0;
    switch (lang) {
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

pub fn globalIdent(self: Self, index: Node.Index) Node.IdentIndex {
    const n = self.node(index);
    switch (n.tag) {
        .@"var" => {
            const v = self.extraData(Node.Var, n.lhs);
            return v.name;
        },
        .override => {
            const override = self.extraData(Node.Override, n.lhs);
            return override.name;
        },
        .@"fn" => {
            const header = self.extraData(Node.FnHeader, n.lhs);
            return header.name;
        },
        .@"const" => {
            const typed_ident = self.extraData(Node.TypedIdent, n.lhs);
            return typed_ident.name;
        },
        .type_alias, .@"struct" => return n.lhs,
        else => return 0,
    }
}

pub fn globalName(self: Self, index: Node.Index) []const u8 {
    const ident = self.globalIdent(index);
    return self.identifier(ident);
}

pub fn removeFromSpan(self: *Self, span_index: Node.Index, item_index: usize) void {
    const n = self.node(span_index);
    const bytes = self.extra[n.lhs + item_index * @sizeOf(Node.Index)..n.rhs];
    var indices = std.mem.bytesAsSlice(Node.Index, bytes);
    for (0..indices.len - 1) |i| indices[i] = indices[i + 1];
    self.nodes.items(.rhs)[span_index] -= @sizeOf(Node.Index);
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

// pub fn resolveNumber(self: Self, index: Node.Index) ?i64 {
//     const n = self.node(index);
//     switch (n.tag) {
//         .ident => {
//             node.lhs = try self.getOrPutRef(file, tree.identifier(node.lhs), node.src_offset);
//             node.rhs = try self.visit(mod, node.rhs);
//         },
//         .true, .false => {},
//         .abstract_int,
//         .i32,
//         .u32,
//         .abstract_float,
//         .f32,
//         .f16,
//         => {},
//     }
// }
