/// Immutable abstract syntax tree.
const std = @import("std");
const node_mod = @import("./Node.zig");
const WgslParsingError = @import("../wgsl/ParsingError.zig");
const Language = @import("../file/File.zig").Language;
const FileError = @import("../file/Error.zig");

const Allocator = std.mem.Allocator;
const Self = @This();
const Node = node_mod.Node;
pub const NodeList = std.MultiArrayList(Node);
pub const Identifiers = std.MultiArrayList(std.StringArrayHashMapUnmanaged(void).Data);

/// Node 0 is a span of directives followed by declarations. Since there can be no
/// references to this root node, 0 is available to indicate null
nodes: NodeList.Slice,
/// For nodes with identifiers
identifiers: Identifiers.Slice,
/// For lists and nodes with too much inseperable data
extra: []Node.Index,
/// For rendering lannguage-dependent errors
from_lang: Language,

pub fn deinit(self: *Self, allocator: Allocator) void {
    for (self.identifiers.items(.key)) |k| allocator.free(k);
    self.identifiers.deinit(allocator);
    self.nodes.deinit(allocator);
    allocator.free(self.extra);
    self.* = undefined;
}

pub fn identifier(self: Self, index: Node.IdentIndex) []const u8 {
    return self.identifiers.get(index).key;
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
        std.debug.assert(self.nodes.items(.tags)[i] == .span);
        const span = self.nodes.get(i).span;
        return self.extra[span.from..span.to];
    }
    return &.{};
}

pub fn hasError(self: Self) bool {
    for (self.spanToList(0)) |i| {
        switch (self.node(i)) {
            .@"error" => return true,
            else => {},
        }
    }
    return false;
}

pub fn renderErrors(
    self: Self,
    writer: anytype,
    term: std.io.tty.Config,
    source: [:0]const u8,
    file_path: ?[]const u8,
) !void {
    for (self.spanToList(0)) |i| {
        switch (self.node(i)) {
            .@"error" => |e| {
                const loc = self.extraData(node_mod.ErrorLoc, e.error_loc);
                try FileError.render(writer, term, source, file_path, loc);
                switch (self.from_lang) {
                    .wgsl => {
                        const tag: WgslParsingError.Tag = @enumFromInt(e.tag);
                        try tag.render(writer, @enumFromInt(e.expected_token_tag));
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
    switch (self.node(index)) {
        .global_var => |n| {
            const global_var = self.extraData(node_mod.GlobalVar, n.global_var);
            return self.identifier(global_var.name);
        },
        .override => |n| {
            const override = self.extraData(node_mod.Override, n.override);
            return self.identifier(override.name);
        },
        .@"fn" => |n| {
            const header = self.extraData(node_mod.FnHeader, n.fn_header);
            return self.identifier(header.name);
        },
        .@"const" => |n| {
            const typed_ident = self.extraData(node_mod.TypedIdent, n.typed_ident);
            return self.identifier(typed_ident.name);
        },
        .type_alias => |n| return self.identifier(n.new_name),
        .@"struct" => |n| return self.identifier(n.name),
        else => return "",
    }
}

pub fn removeFromSpan(self: *Self, span_index: Node.Index, item_index: usize) void {
    self.nodes.items(.data)[span_index].span.orderedRemove(self.extra, item_index);
}

pub fn modName(self: Self, imp: Node.Import) []const u8 {
    const extra = self.extraData(node_mod.Import, imp.import);
    return self.identifier(extra.module);
}
