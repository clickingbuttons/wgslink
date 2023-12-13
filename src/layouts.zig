const std = @import("std");
const BindGroupLayouts = @import("./ast/BindGroupLayouts.zig");
const StructLayouts = @import("./ast/StructLayouts.zig");
const Ast = @import("./ast/Ast.zig");

const Self = @This();
const Allocator = std.mem.Allocator;

bind_groups: BindGroupLayouts,
structs: StructLayouts,

pub fn init(allocator: Allocator, tree: Ast) !Self {
    return Self{
        .bind_groups = try BindGroupLayouts.extractLayouts(allocator, tree),
        .structs = try StructLayouts.extractLayouts(allocator, tree),
    };
}

pub fn deinit(self: *Self) void {
    self.bind_groups.deinit();
    self.structs.deinit();
}
