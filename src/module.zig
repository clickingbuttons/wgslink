const std = @import("std");
const Ast = @import("./wgsl/Ast.zig");
const ErrorList = @import("./wgsl/ErrorList.zig");

const Allocator = std.Allocator;
const Self = @This();

source: []const u8,
ast: *Ast,
deps: []Self,

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
        if (err == error.Parsing) try errors.print(source, null);
        return err;
    };
    return Self{
        .source = source,
        .ast = ast,
        .deps = &.{},
    };
}

pub fn deinit(self: *Self) void {
    self.ast.deinit();
}
