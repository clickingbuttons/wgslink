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
};
const StringSet = std.StringArrayHashMapUnmanaged(void);
const ImportTable = std.StringArrayHashMapUnmanaged(StringSet);

allocator: Allocator,
name: []const u8,
status: enum {
    empty,
    read,
    parsed,
} = .empty,
stat: ?Stat = null,
source: ?[:0]const u8 = null,
tree: ?Ast = null,
import_table: ImportTable = .{},

pub fn deinit(self: *Self) void {
    var allocator = self.allocator;
    if (self.tree) |*t| t.deinit(allocator);
    if (self.source) |s| allocator.free(s);
    var iter =self.import_table.iterator();
    while (iter.next()) |kv| {
        allocator.free(kv.key_ptr.*);
        kv.value_ptr.*.deinit(allocator);
    }
    self.import_table.deinit(allocator);
}

pub fn read(self: *Self) !void {
    std.debug.print("read {s}\n", .{self.name});
    const path = self.name;
    var source_file = try std.fs.cwd().openFile(path, .{});
    defer source_file.close();

    const stat = try source_file.stat();
    if (stat.size > std.math.maxInt(u32))
        return error.FileTooBig;

    const source = try self.allocator.allocSentinel(u8, @as(usize, @intCast(stat.size)), 0);
    const amt = try source_file.readAll(source);
    if (amt != stat.size)
        return error.UnexpectedEndOfFile;

    self.status = .read;
    self.stat = Stat{
        .inode = stat.inode,
        .size = stat.size,
        .mtime = stat.mtime,
    };
    self.source = source;
}

pub fn parse(self: *Self, tree_shake: ?TreeShaker.Options) !void {
    std.debug.print("parse {s}\n", .{self.name});
    if (self.status != .read) return;
    var tree = try Ast.init(self.allocator, self.source.?);
    if (tree.errors.len > 0) {
        const stderr = std.io.getStdErr();
        const term = std.io.tty.detectConfig(stderr);
        try stderr.writer().writeByte('\n');
        for (tree.errors) |e| try tree.renderError(e, stderr.writer(), term);
    } else {
        if (tree_shake) |opts| try TreeShaker.treeShake(&tree, self.allocator,  opts);
        const dirname = std.fs.path.dirname(self.name).?;
        for (tree.spanToList(0)) |node| {
            if (tree.nodeTag(node) != .import) continue;
            const token = tree.tokenSource(tree.nodeRHS(node));
            const mod_name = token[1 .. token.len - 1];
            const resolved = try std.fs.path.resolve(self.allocator, &[_][]const u8{ dirname, mod_name });

            const mod_imports = try self.import_table.getOrPut(self.allocator, resolved);
            if (!mod_imports.found_existing) mod_imports.value_ptr.* = StringSet{};
            const lhs = tree.nodeLHS(node);
            if (lhs != 0) {
                for (tree.spanToList(lhs)) |n| {
                    if (tree.nodeTag(n) == .empty) continue;
                    try mod_imports.value_ptr.*.put(self.allocator, tree.nodeSource(n), {});
                }
            }

        }
        self.status = .parsed;
        self.tree = tree;
    }
}
