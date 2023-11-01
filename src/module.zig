const std = @import("std");
const Ast = @import("./wgsl/Ast.zig");
const ErrorList = @import("./wgsl/ErrorList.zig");

const Allocator = std.Allocator;
const Self = @This();
pub const File = struct {
    pub const Stat = struct {
        inode: std.fs.File.INode,
        size: u64,
        mtime: i128,
    };
    status: enum {
        never_loaded,
        retryable_failure,
        parse_failure,
        astgen_failure,
        success,
    },
    source_loaded: bool,
    tree_loaded: bool,
    /// Whether this is populated depends on `source_loaded`.
    source: [:0]const u8,
    /// Whether this is populated depends on `status`.
    stat: Stat,
    /// Whether this is populated or not depends on `tree_loaded`.
    tree: Ast,
};

allocator: Allocator,
source: []const u8,
ast: *Ast,
/// Keys are fully resolved file paths. This table owns the keys and values.
import_table: std.StringArrayHashMapUnmanaged(*File) = .{},

pub fn init(allocator: Allocator, path: []const u8) !Self {
    var errors = try ErrorList.init(allocator);
    defer errors.deinit();

    var source_file = try std.fs.cwd().openFile(path, .{});
    defer source_file.close();

    const stat = try source_file.stat();
    if (stat.size > std.math.maxInt(u32))
        return error.FileTooBig;

    const source = try allocator.allocSentinel(u8, @as(usize, @intCast(stat.size)), 0);
    const amt = try source_file.readAll(source);
    if (amt != stat.size)
        return error.UnexpectedEndOfFile;

    var ast = Ast.init(allocator, &errors, source) catch |err| {
        if (err == error.Parsing) try errors.print(source, path);
        return err;
    };
    return Self{
        .allocator = allocator,
        .source = source,
        .ast = ast,
        .deps = &.{},
    };
}

pub fn deinit(self: *Self) void {
    self.ast.deinit();
}
