const std = @import("std");
pub const Loc = @import("./Loc.zig");
const Ast = @import("../ast/Ast.zig");
const TreeShaker = @import("../ast/TreeShaker.zig");
const Parser = @import("../wgsl/Parser.zig");
const Node = @import("../ast/Node.zig");

pub const Error = @import("./Error.zig");
pub const Language = enum {
    wgsl,

    pub fn fromPath(path: []const u8) ?@This() {
        if (std.mem.endsWith(u8, path, ".wgsl")) return .wgsl;

        return null;
    }
};
pub const max_size = std.math.maxInt(Loc.Index);
pub const Stat = struct {
    inode: std.fs.File.INode,
    size: u64,
    mtime: i128,

    pub fn init(file: std.fs.File) !@This() {
        const s = try file.stat();
        if (s.size > max_size) return error.FileTooBig;
        return .{
            .inode = s.inode,
            .size = s.size,
            .mtime = s.mtime,
        };
    }

    pub fn eql(self: Stat, o: Stat) bool {
        return self.inode == o.inode and self.size == o.size and self.mtime == o.mtime;
    }
};
const Allocator = std.mem.Allocator;
const Self = @This();

allocator: Allocator,
path: []const u8,
source_file: ?std.fs.File = null,
stat: ?Stat = null,
source: ?[:0]const u8 = null,
tree: ?Ast = null,

pub fn load(self: *Self) !void {
    const language = Language.fromPath(self.path) orelse return error.UnsupportedLanguage;

    if (self.source_file == null) self.source_file = try std.fs.cwd().openFile(self.path, .{});

    const new_stat = try Stat.init(self.source_file.?);
    if (self.stat != null and self.stat.?.eql(new_stat)) return;
    self.stat = new_stat;

    const size = self.stat.?.size;
    var source = try self.allocator.allocSentinel(u8, size, 0);
    const amt = try self.source_file.?.readAll(source);
    if (amt != size) return error.UnexpectedEndOfFile;
    self.source = source;

    self.tree = switch (language) {
        .wgsl => try Parser.parse(self.allocator, self.path, self.source.?),
    };
}

pub fn deinit(self: *Self) void {
    const allocator = self.allocator;
    if (self.tree) |*t| t.deinit(allocator);
    if (self.source) |s| self.allocator.free(s);
    if (self.source_file) |*f| f.close();
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

pub fn makeError(self: Self, tok_start: Loc.Index, tok_end: Loc.Index, data: Error.Data) Error {
    const tree = self.tree.?;
    const loc = Error.ErrorLoc.init(tree.newlines, tok_start, tok_end);
    return .{
        .path = self.path,
        .source = self.source.?,
        .loc = loc,
        .data = data,
    };
}

pub fn makeErrorAdvanced(
    self: Self,
    src_offset: Loc.Index,
    token: anytype,
    data: Error.Data,
) Error {
    const loc = self.tree.?.getErrorLoc(self.source.?, src_offset, token);
    return .{
        .path = self.path,
        .source = self.source.?,
        .loc = loc,
        .data = data,
    };
}
