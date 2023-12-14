const std = @import("std");
const Ast = @import("./Ast.zig");
const Node = @import("./Node.zig");
const renderer = @import("../WgslRenderer.zig").renderer;

const Self = @This();
const Allocator = std.mem.Allocator;
const StructMember = struct {
    offset: usize,
    type: []const u8,
    size: usize,
};
const StructLayout = std.StringArrayHashMapUnmanaged(StructMember);
const StructLayouts = std.StringHashMapUnmanaged(StructLayout);

allocator: Allocator,
layouts: StructLayouts = .{},

pub fn deinit(self: *Self) void {
    var iter = self.layouts.valueIterator();
    while (iter.next()) |v| v.deinit(self.allocator);
    self.layouts.deinit(self.allocator);
}

pub fn jsonStringify(self: Self, jw: anytype) !void {
    try jw.beginObject();
    var iter = self.layouts.iterator();
    while (iter.next()) |kv| {
        try jw.objectField(kv.key_ptr.*);
        try jw.beginObject();
        var iter2 = kv.value_ptr.*.iterator();
        while (iter2.next()) |kv2| {
            try jw.objectField(kv2.key_ptr.*);
            try jw.write(kv2.value_ptr.*);
        }
        try jw.endObject();
    }
    try jw.endObject();
}

pub fn extractLayouts(allocator: Allocator, tree: Ast) !Self {
    var res = Self{ .allocator = allocator };
    for (tree.spanToList(0)) |i| try visit(&res, tree, i);
    return res;
}

const AlignSize = struct {
    @"align": usize = 0,
    size: usize = 0,
};

fn render(tree: Ast, index: Node.Index) !usize {
    const node = tree.node(index);
    return switch (node.tag) {
        // .number => {
        //     const n = tree.identifier(node.lhs);
        //     return try std.fmt.parseUnsigned(usize, n, 10);
        // },
        else => error.AlignOrSizeExpression,
    };
}

fn getAlignSize(tree: Ast, index: Node.Index, ty: []const u8) !AlignSize {
    _ = ty;
    var res = AlignSize{};
    if (index != 0) {
        for (tree.spanToList(index)) |i| {
            const node = tree.node(i);
            std.debug.assert(node.tag == .attribute);
            const attr: Node.Attribute = @enumFromInt(node.lhs);
            switch (attr) {
                .@"align" => res.@"align" = try render(tree, node.rhs),
                .size => res.size = try render(tree, node.rhs),
                else => {},
            }
        }
    }
    return res;
}

/// Caller owns returned slice
fn getType(self: *Self, tree: Ast, index: Node.Index) ![]const u8 {
    var res = std.ArrayList(u8).init(self.allocator);
    const node = tree.node(index);
    std.debug.assert(node.tag == .ident or node.tag == .type);

    var wgsl = renderer(res.writer(), .{});
    try wgsl.writeNode(tree, node);

    return res.toOwnedSlice();
}

fn getLayout(self: *Self, tree: Ast, index: Node.Index) !StructLayout {
    const members = tree.spanToList(index);
    var res = StructLayout{};
    try res.ensureTotalCapacity(self.allocator, members.len);
    const offset: usize = 0;
    for (members) |i| {
        const node = tree.node(i);

        const typed_ident = tree.extraData(Node.TypedIdent, node.rhs);
        const name = tree.identifier(typed_ident.name);
        const ty = try self.getType(tree, typed_ident.type);
        const align_size = try getAlignSize(tree, node.lhs, ty);

        const gop = res.getOrPutAssumeCapacity(name);
        if (gop.found_existing) return error.MemberAlreadyDefined;
        gop.value_ptr.* = StructMember{
            .offset = offset,
            .type = ty,
            .size = align_size.size,
        };
    }
    return res;
}

fn visit(self: *Self, tree: Ast, index: Node.Index) !void {
    const node = tree.node(index);

    switch (node.tag) {
        .@"struct" => {
            const name = tree.identifier(node.lhs);
            const layout = try self.getLayout(tree, node.rhs);
            try self.layouts.putNoClobber(self.allocator, name, layout);
        },
        else => {},
    }
}
