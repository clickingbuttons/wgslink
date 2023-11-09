const std = @import("std");
const Ast = @import("./wgsl/Ast.zig");
const Node = @import("./wgsl/Node.zig");

const Allocator = std.mem.Allocator;
const Self = @This();
pub const Stat = struct {
    inode: std.fs.File.INode,
    size: u64,
    mtime: i128,
};
const ImportTable = std.StringArrayHashMapUnmanaged(void);

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

pub fn parse(self: *Self) !void {
    std.debug.print("parse {s}\n", .{self.name});
    if (self.status != .read) return;
    var tree = try Ast.init(self.allocator, self.source.?);
    if (tree.errors.len > 0) {
        const stderr = std.io.getStdErr();
        const term = std.io.tty.detectConfig(stderr);
        try stderr.writer().writeByte('\n');
        for (tree.errors) |e| try tree.renderError(e, stderr.writer(), term);
    } else {
        const dirname = std.fs.path.dirname(self.name).?;
        // TODO: only used imports
        for (tree.spanToList(0)) |node| {
            if (tree.nodeTag(node) != .import) continue;
            const token = tree.tokenSource(tree.nodeRHS(node));
            const mod_name = token[1 .. token.len - 1];
            const resolved = try std.fs.path.resolve(self.allocator, &[_][]const u8{ dirname, mod_name });
            try self.import_table.put(self.allocator, resolved, {});
        }
        self.status = .parsed;
        self.tree = tree;
    }
}

pub fn deinit(self: *Self) void {
    var allocator = self.allocator;
    if (self.tree) |*t| t.deinit(allocator);
    if (self.source) |s| allocator.free(s);
    for (self.import_table.keys()) |k| allocator.free(k);
    self.import_table.deinit(allocator);
}
