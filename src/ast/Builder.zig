const std = @import("std");
const Ast = @import("./Ast.zig");
const Node = @import("./Node.zig").Node;
const Language = @import("../file/File.zig").Language;

const Self = @This();
const Allocator = std.mem.Allocator;

/// Main data structure
nodes: Ast.NodeList = .{},
/// Nodes with identifers store indexes into here.
/// For `var foo: u32 = baz();` this will store `foo`, `u32`, and `baz`
/// Owns the strings so that `source` may be freed after parsing is finished.
identifiers: std.StringArrayHashMapUnmanaged(void) = .{},
/// For nodes with more data than @sizeOf(Node)
extra: std.ArrayListUnmanaged(Node.Index) = .{},

pub fn deinit(self: *Self, allocator: Allocator) void {
    self.nodes.deinit(allocator);
    for (self.identifiers.keys()) |k| allocator.free(k);
    self.identifiers.deinit(allocator);
    self.extra.deinit(allocator);
}

pub fn listToSpan(
    self: *Self,
    allocator: Allocator,
    list: []const Node.Index,
) Allocator.Error!Node.Index {
    if (list.len == 0) return 0;
    try self.extra.appendSlice(allocator, list);
    return try self.addNode(allocator, Node{ .span = .{
        .from = @intCast(self.extra.items.len - list.len),
        .to = @intCast(self.extra.items.len),
    } });
}

pub fn addNode(self: *Self, allocator: Allocator, node: Node) Allocator.Error!Node.Index {
    const i: Node.Index = @intCast(self.nodes.len);
    try self.nodes.append(allocator, node);
    return i;
}

pub fn addExtra(self: *Self, allocator: Allocator, extra: anytype) Allocator.Error!Node.Index {
    const fields = std.meta.fields(@TypeOf(extra));
    try self.extra.ensureUnusedCapacity(allocator, fields.len);
    const result: Node.Index = @intCast(self.extra.items.len);
    inline for (fields) |field| {
        self.extra.appendAssumeCapacity(@field(extra, field.name));
    }
    return result;
}

pub fn toOwnedAst(self: *Self, allocator: Allocator, lang: Language) Allocator.Error!Ast {
    return Ast{
        .nodes = self.nodes.toOwnedSlice(),
        .identifiers = self.identifiers.entries.toOwnedSlice(),
        .extra = try self.extra.toOwnedSlice(allocator),
        .from_lang = lang,
    };
}

pub fn getOrPutIdent(
    self: *Self,
    allocator: Allocator,
    ident: []const u8,
) Allocator.Error!Node.IdentIndex {
    const gop = try self.identifiers.getOrPut(allocator, ident);
    if (!gop.found_existing) {
        gop.key_ptr.* = try allocator.dupe(u8, ident);
    }
    return @intCast(gop.index);
}
