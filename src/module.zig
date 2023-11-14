const std = @import("std");
const Ast = @import("./wgsl/Ast.zig");
const Node = @import("./wgsl/Node.zig");
const TreeShaker = @import("tree_shaker.zig");

const Allocator = std.mem.Allocator;
const Self = @This();
pub const Stat = struct {
    inode: std.fs.File.INode,
    size: u64,
    mtime: i128,

    pub fn eql(self: Stat, o: Stat) bool {
        return self.inode == o.inode and self.size == o.size and self.mtime == o.mtime;
    }
};
const StringSet = std.StringArrayHashMapUnmanaged(void);
const ImportTable = std.StringArrayHashMapUnmanaged(StringSet);
pub const File = struct {
    stat: Stat,
    source: [:0]const u8,
    tree: Ast,
    import_table: ImportTable,

    pub fn init(allocator: Allocator, path: []const u8, stat: Stat, source: [:0]const u8, tree_shake: ?TreeShaker.Options) !File {
        var tree = try Ast.init(allocator, source);

        var import_table = ImportTable{};
        if (tree_shake) |opts| try TreeShaker.treeShake(&tree, allocator, opts);
        for (tree.spanToList(0)) |node| {
            if (tree.nodeTag(node) != .import) continue;

            const mod_name = tree.moduleName(node);
            const resolved = try resolveFrom(allocator, path, mod_name);
            const mod_imports = try import_table.getOrPut(allocator, resolved);
            if (!mod_imports.found_existing) mod_imports.value_ptr.* = StringSet{};
            const lhs = tree.nodeLHS(node);
            if (lhs != 0) {
                for (tree.spanToList(lhs)) |n| {
                    if (tree.nodeTag(n) == .empty) continue;
                    try mod_imports.value_ptr.*.put(allocator, tree.nodeSource(n), {});
                }
            }
        }
        return File{
            .stat = stat,
            .source = source,
            .tree = tree,
            .import_table = import_table,
        };
    }

    pub fn deinit(self: *File, allocator: Allocator) void {
        self.tree.deinit(allocator);
        allocator.free(self.source);
        var iter = self.import_table.iterator();
        while (iter.next()) |kv| {
            allocator.free(kv.key_ptr.*);
            kv.value_ptr.*.deinit(allocator);
        }
        self.import_table.deinit(allocator);
    }
};

allocator: Allocator,
name: []const u8,
imported_by: []const u8,
parsed: bool = false,
file: ?File = null,

pub fn deinit(self: *Self) void {
    if (self.file) |*f| f.deinit(self.allocator);
}

pub fn init(self: *Self, tree_shake: ?TreeShaker.Options) !void {
    const path = self.name;

    var source_file = try std.fs.cwd().openFile(path, .{});
    defer source_file.close();
    const stat = brk: {
        const s = try source_file.stat();
        if (s.size > std.math.maxInt(u32)) return error.FileTooBig;
        break :brk Stat{
            .inode = s.inode,
            .size = s.size,
            .mtime = s.mtime,
        };
    };

    if (self.file) |f| if (f.stat.eql(stat)) return;

    const source = try self.allocator.allocSentinel(u8, @as(usize, @intCast(stat.size)), 0);
    const amt = try source_file.readAll(source);
    if (amt != stat.size) return error.UnexpectedEndOfFile;

    self.file = try File.init(self.allocator, path, stat, source, tree_shake);
    if (self.file.?.tree.errors.len > 0) return error.Parsing;
    self.parsed = true;
}

pub fn render(self: Self, renderer: anytype) !void {
    if (self.file) |f| {
        if (!renderer.minify) try renderer.print("// {s}\n", .{self.name});
        try renderer.writeTranslationUnit(f.tree);
        try renderer.newline();
    }
}

/// Caller owns returned slice
pub fn resolveFrom(allocator: Allocator, from_file: []const u8, relpath: []const u8) ![]const u8 {
    const dirname = std.fs.path.dirname(from_file).?;
    return try std.fs.path.resolve(allocator, &[_][]const u8{ dirname, relpath });
}

/// Caller owns returned slice
pub fn resolve(self: Self, relpath: []const u8) ![]const u8 {
    return try resolveFrom(self.allocator, self.name, relpath);
}
