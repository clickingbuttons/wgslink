const std = @import("std");
const Ast = @import("./wgsl/Ast.zig");
const Node = @import("./wgsl/Node.zig");

const Allocator = std.mem.Allocator;
const Used = std.StringHashMap(void);

/// Minimizes by modifying existing nodes.
pub fn minimize(tree: *Ast, allocator: Allocator) !void {}
