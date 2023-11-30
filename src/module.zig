const std = @import("std");
const Ast = @import("./ast/Ast.zig");
const Node = @import("./ast/Node.zig").Node;
const TreeShaker = @import("./ast/TreeShaker.zig");
const Parser = @import("./wgsl/Parser.zig");
const File = @import("./file/File.zig");

pub const Error = error{UnsupportedLanguage};
pub const resolveFrom = File.resolveFrom;
const Allocator = std.mem.Allocator;
const Self = @This();

allocator: Allocator,
path: []const u8,
imported_by: []const u8,
file: ?File = null,

fn getLanguage(path: []const u8) ?File.Language {
    if (std.mem.endsWith(u8, path, ".wgsl")) return .wgsl;

    return null;
}

pub fn init(allocator: Allocator, path: []const u8, imported_by: []const u8) Self {
    return Self{ .allocator = allocator, .path = path, .imported_by = imported_by };
}

fn sourceAdvanced(self: Self, size: usize) ![:0]const u8 {
    // TODO: mmap so errors don't have to read whole file
    var source_file = try std.fs.cwd().openFile(self.path, .{});
    defer source_file.close();

    const res = try self.allocator.allocSentinel(u8, size, 0);
    errdefer self.allocator.free(res);

    const amt = try source_file.readAll(res);
    if (amt != size) return error.UnexpectedEndOfFile;

    return res;
}

/// Caller owns returned memory
pub fn source(self: Self) ![:0]const u8 {
    return try self.sourceAdvanced(self.file.?.stat.size);
}

pub fn load(self: *Self, tree_shake: ?TreeShaker.Options) !void {
    const language = getLanguage(self.path) orelse return Error.UnsupportedLanguage;

    var source_file = try std.fs.cwd().openFile(self.path, .{});
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

    const file_source = try self.sourceAdvanced(stat.size);

    self.file = try File.init(
        self.allocator,
        self.path,
        stat,
        language,
        file_source,
        tree_shake,
    );
    if (self.file.?.tree.hasError()) return error.Parsing;
}

pub fn deinit(self: *Self) void {
    if (self.file) |*f| f.deinit(self.allocator);
}

pub fn hasError(self: Self) bool {
    if (self.file) |f| return f.tree.hasError();
    return false;
}

pub fn renderErrors(self: Self, writer: anytype, config: std.io.tty.Config) !void {
    const file_source = try self.source();
    if (self.file) |f| try f.renderErrors(writer, config, self.path, file_source);
}

/// Caller owns returned slice
pub fn resolve(self: Self, relpath: []const u8) ![]const u8 {
    return try File.resolveFrom(self.allocator, self.path, relpath);
}
