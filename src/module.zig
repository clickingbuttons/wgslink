const std = @import("std");
const Ast = @import("./ast/Ast.zig");
const Node = @import("./ast/Node.zig").Node;
const TreeShaker = @import("./ast/TreeShaker.zig");
const Parser = @import("./wgsl/Parser.zig");
const File = @import("./file/File.zig");

const Allocator = std.mem.Allocator;
const Self = @This();
pub const resolveFrom = File.resolveFrom;

allocator: Allocator,
name: []const u8,
imported_by: []const u8,
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
        if (s.size > File.max_size) return error.FileTooBig;
        break :brk File.Stat{
            .inode = s.inode,
            .size = s.size,
            .mtime = s.mtime,
        };
    };

    if (self.file) |f| if (f.stat.eql(stat)) return;

    const source = try self.allocator.allocSentinel(u8, stat.size, 0);
    defer self.allocator.free(source);
    const amt = try source_file.readAll(source);
    if (amt != stat.size) return error.UnexpectedEndOfFile;

    self.file = try File.init(self.allocator, path, stat, source, tree_shake);
    if (self.file.?.tree.hasError()) return error.Parsing;
}

pub fn hasError(self: Self) bool {
    if (self.file) |f| return f.tree.hasError();
    return false;
}

pub fn renderErrors(self: Self, errwriter: anytype, errconfig: std.io.tty.Config) !void {
    if (self.file) |f| try f.renderErrors(errwriter, errconfig);
}

/// Caller owns returned slice
pub fn resolve(self: Self, relpath: []const u8) ![]const u8 {
    return try File.resolveFrom(self.allocator, self.name, relpath);
}
