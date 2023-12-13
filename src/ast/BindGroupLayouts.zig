const std = @import("std");
const Ast = @import("./Ast.zig");
const Node = @import("./Node.zig");

const Self = @This();
const Allocator = std.mem.Allocator;
const BufferLayout = struct { type: enum { uniform, storage, @"read-only-storage" } };
const SamplerLayout = struct { type: enum { filtering, @"non-filtering", comparison } };
const TextureLayout = struct { sampleType: enum { float, @"unfilterable-float", depth, sint, uint } };
const BindGroupLayout = struct {
    binding: []const u8,
    buffer: ?BufferLayout = null,
    sampler: ?SamplerLayout = null,
    texture: ?TextureLayout = null,
};
const VarLayouts = std.StringHashMapUnmanaged(BindGroupLayout);
const BindGroupLayouts = std.StringHashMapUnmanaged(VarLayouts);

allocator: Allocator,
layouts: BindGroupLayouts = .{},

pub fn deinit(self: *Self) void {
    var iter = self.layouts.valueIterator();
    while (iter.next()) |v| v.deinit(self.allocator);
    self.layouts.deinit(self.allocator);
}

pub fn jsonStringify(self: Self, jw: anytype) !void {
    try jw.beginObject();
    var group_iter = self.layouts.iterator();
    while (group_iter.next()) |kv| {
        try jw.objectField(kv.key_ptr.*);
        try jw.beginObject();
        var var_iter = kv.value_ptr.*.iterator();
        while (var_iter.next()) |kv2| {
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

const GroupBinding = struct {
    group: ?[]const u8 = null,
    binding: ?[]const u8 = null,
};

fn render(tree: Ast, index: Node.Index) ![]const u8 {
    const node = tree.node(index);
    return switch (node.tag) {
        .type, .ident => tree.identifier(node.lhs),
        else => error.GroupOrBindingExpression,
    };
}

fn getGroupBinding(tree: Ast, index: Node.Index) !GroupBinding {
    var res = GroupBinding{};
    for (tree.spanToList(index)) |i| {
        const node = tree.node(i);
        std.debug.assert(node.tag == .attribute);
        const attr: Node.Attribute = @enumFromInt(node.lhs);
        switch (attr) {
            .group => {
                if (res.group) |_| return error.DuplicateGroup;
                res.group = try render(tree, node.rhs);
            },
            .binding => {
                if (res.binding) |_| return error.DuplicateBinding;
                res.binding = try render(tree, node.rhs);
            },
            else => {},
        }
    }
    return res;
}

fn visit(self: *Self, tree: Ast, index: Node.Index) !void {
    const node = tree.node(index);

    switch (node.tag) {
        .global_var => {
            const global_var = tree.extraData(Node.GlobalVar, node.lhs);
            const group_binding = try getGroupBinding(tree, global_var.attrs);
            if (global_var.address_space == 0) return;
            if (group_binding.group == null or group_binding.binding == null) return;

            const gop = try self.layouts.getOrPut(self.allocator, group_binding.group.?);
            if (!gop.found_existing) gop.value_ptr.* = VarLayouts{};

            var var_layout = BindGroupLayout{ .binding = group_binding.binding.? };
            const address_space: Node.AddressSpace = @enumFromInt(global_var.address_space);
            const name = tree.identifier(global_var.name);
            switch (address_space) {
                .uniform => {
                    var_layout.buffer = .{ .type = .uniform };
                },
                .storage => {
                    const access_mode: Node.AccessMode = @enumFromInt(global_var.access_mode);
                    var_layout.buffer = .{
                        .type = if (access_mode == .read) .@"read-only-storage" else .storage,
                    };
                },
                else => {},
                // const type = try self.visit(mod, global_var.type);
            }
            try gop.value_ptr.*.put(self.allocator, name, var_layout);
        },
        else => {},
    }
}
