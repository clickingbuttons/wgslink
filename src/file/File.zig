const std = @import("std");
const Loc = @import("./Loc.zig");
const Ast = @import("../ast/Ast.zig");
const TreeShaker = @import("../ast/TreeShaker.zig");
const Parser = @import("../wgsl/Parser.zig");
const node_mod = @import("../ast/Node.zig");

pub const Language = enum { wgsl };
pub const Stat = struct {
    inode: std.fs.File.INode,
    size: u64,
    mtime: i128,

    pub fn eql(self: Stat, o: Stat) bool {
        return self.inode == o.inode and self.size == o.size and self.mtime == o.mtime;
    }
};
pub const max_size = std.math.maxInt(Loc.Index);
const StringSet = std.StringArrayHashMapUnmanaged(void);
const ImportTable = std.StringArrayHashMapUnmanaged(StringSet);
const Allocator = std.mem.Allocator;
const Self = @This();

stat: Stat,
tree: Ast,
import_table: ImportTable,

pub fn init(
    allocator: Allocator,
    path: []const u8,
    stat: Stat,
    language: Language,
    source: [:0]const u8,
    tree_shake: ?TreeShaker.Options,
) !Self {
    var tree = switch (language) {
        .wgsl => try Parser.parse(allocator, source),
    };
    if (tree_shake) |opts| try TreeShaker.treeShake(allocator, &tree, opts);
    const import_table = try importTable(allocator, path, tree);
    return Self{
        .stat = stat,
        .tree = tree,
        .import_table = import_table,
    };
}

pub fn deinit(self: *Self, allocator: Allocator) void {
    self.tree.deinit(allocator);
    var iter = self.import_table.iterator();
    while (iter.next()) |kv| {
        allocator.free(kv.key_ptr.*);
        kv.value_ptr.*.deinit(allocator);
    }
    self.import_table.deinit(allocator);
}

/// Caller owns returned slice
pub fn resolveFrom(
    allocator: Allocator,
    from_file: []const u8,
    relpath: []const u8,
) ![]const u8 {
    const dirname = std.fs.path.dirname(from_file).?;
    return try std.fs.path.resolve(allocator, &[_][]const u8{ dirname, relpath });
}

fn importTable(allocator: Allocator, path: []const u8, tree: Ast) !ImportTable {
    var res = ImportTable{};

    for (tree.spanToList(0)) |r| {
        switch (tree.node(r)) {
            .import => |n| {
                const mod_name = tree.modName(n);
                const resolved = try resolveFrom(allocator, path, mod_name);
                const mod_imports = try res.getOrPut(allocator, resolved);
                if (!mod_imports.found_existing) mod_imports.value_ptr.* = StringSet{};

                const aliases = tree.spanToList(if (n.aliases != 0) n.aliases else null);
                for (aliases) |j| {
                    switch (tree.node(j)) {
                        .import_alias => |a| {
                            const mod_symbol = tree.identifier(a.old);
                            try mod_imports.value_ptr.*.put(allocator, mod_symbol, {});
                        },
                        else => {},
                    }
                }
            },
            else => {},
        }
    }

    return res;
}

pub fn renderErrors(
    self: Self,
    writer: anytype,
    config: std.io.tty.Config,
    path: []const u8,
    source: [:0]const u8,
) !void {
    try self.tree.renderErrors(writer, config, source, path);
}
