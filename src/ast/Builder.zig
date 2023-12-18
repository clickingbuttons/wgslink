const std = @import("std");
const builtin = @import("builtin");
const Ast = @import("./Ast.zig");
const Node = @import("./Node.zig");
const Language = @import("../file/File.zig").Language;
const File = @import("../file/File.zig");

const Self = @This();
const Allocator = std.mem.Allocator;
const Loc = File.Loc;

// See ./Ast.zig
nodes: Ast.NodeList,
identifiers: Ast.StringSet = .{},
extra: std.ArrayListAlignedUnmanaged(u8, @alignOf(Node.Index)) = .{},
newlines: std.ArrayListUnmanaged(Loc.Index) = .{},
errors: std.ArrayListUnmanaged(File.Error) = .{},

pub fn init(allocator: Allocator) !Self {
    var nodes = Ast.NodeList{};
    // Root node must be index 0.
    try nodes.append(allocator, Node{
        .src_offset = 0,
        .tag = .span,
        .lhs = 0,
        .rhs = 0,
    });

    return .{ .nodes = nodes };
}

pub fn deinit(self: *Self, allocator: Allocator) void {
    self.nodes.deinit(allocator);
    for (self.identifiers.keys()) |k| allocator.free(k);
    self.identifiers.deinit(allocator);
    self.extra.deinit(allocator);
    self.newlines.deinit(allocator);
    self.errors.deinit(allocator);
}

pub fn listToSpan(
    self: *Self,
    allocator: Allocator,
    src_offset: Loc.Index,
    list: []const Node.Index,
) !Node.Index {
    if (list.len == 0) return 0;
    // pad to align to @sizeOf(Node.Index)
    try self.extra.appendNTimes(allocator, 0, self.extra.items.len % @sizeOf(Node.Index));

    const bytes = std.mem.sliceAsBytes(list);
    try self.extra.appendSlice(allocator, bytes);
    const lhs: Node.Index = @intCast(self.extra.items.len - list.len * @sizeOf(Node.Index));
    const rhs: Node.Index = @intCast(self.extra.items.len);
    Ast.checkSpan(lhs, rhs);

    return try self.addNode(allocator, Node{
        .src_offset = src_offset,
        .tag = .span,
        .lhs = lhs,
        .rhs = rhs,
    });
}

pub fn addNode(self: *Self, allocator: Allocator, node: Node) !Node.Index {
    const i: Node.Index = @intCast(self.nodes.len);
    try self.nodes.append(allocator, node);
    return i;
}

pub fn addExtra(self: *Self, allocator: Allocator, extra: anytype) !Node.ExtraIndex {
    const index: Node.Index = @intCast(self.extra.items.len);
    const bytes = std.mem.toBytes(extra);
    // std.debug.print("write {d} size={d} {}\n", .{ index, bytes.len, extra  });
    try self.extra.appendSlice(allocator, &bytes);
    return index;
}

pub fn toOwned(self: *Self, allocator: Allocator) !Ast {
    return Ast{
        .nodes = self.nodes.toOwnedSlice(),
        .identifiers = self.identifiers.entries.toOwnedSlice(),
        .extra = try self.extra.toOwnedSlice(allocator),
        .newlines = try self.newlines.toOwnedSlice(allocator),
        .errors = try self.errors.toOwnedSlice(allocator),
    };
}

pub fn getOrPutIdent(
    self: *Self,
    allocator: Allocator,
    ident: []const u8,
) !Node.IdentIndex {
    const gop = try self.identifiers.getOrPut(allocator, ident);
    if (!gop.found_existing) {
        gop.key_ptr.* = try allocator.dupe(u8, ident);
    }
    return @intCast(gop.index + 1);
}

pub fn finishRootSpan(self: *Self, allocator: Allocator, roots: []const []const Node.Index) !void {
    var total_len: usize = 0;

    for (roots) |l| {
        const bytes = std.mem.sliceAsBytes(l);
        try self.extra.appendSlice(allocator, bytes);
        total_len += bytes.len;
    }

    const lhs: Node.Index = @intCast(self.extra.items.len - total_len);
    const rhs: Node.Index = @intCast(self.extra.items.len);
    Ast.checkSpan(lhs, rhs);

    self.nodes.items(.lhs)[0] = lhs;
    self.nodes.items(.rhs)[0] = rhs;
}

test "spans" {
    const allocator = std.testing.allocator;
    var builder = try Self.init(allocator);
    defer builder.deinit(allocator);

    _ = try builder.addExtra(allocator, .{
        .a = @as(u8, 2),
        .b = @as(u8, 2),
        .c = @as(u8, 2),
    });

    const indices = [_]Node.Index{ 2, 4, 6, 8 };
    try builder.finishRootSpan(allocator, &[_][]const Node.Index{&indices});

    var ast = try builder.toOwned(allocator);
    defer ast.deinit(allocator);

    try std.testing.expectEqualSlices(Node.Index, &indices, ast.spanToList(0));
}
