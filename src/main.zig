const std = @import("std");
const clap = @import("clap");
const Ast = @import("./wgsl/Ast.zig");
const Module = @import("./module.zig");
const Bundler = @import("./bundler.zig");
// const Pruner = @import("./wgsl/Pruner.zig");

const ThreadPool = std.Thread.Pool;
const max_source = 1 << 30;
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

pub fn main() !void {
    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit.
        \\<str>...               Entry WGSL files
        \\
    );
    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{ .diagnostic = &diag }) catch |err| {
        diag.report(stderr, err) catch {};
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0 or res.positionals.len == 0)
        return clap.help(stderr, clap.Help, &params, .{});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var thread_pool: ThreadPool = undefined;
    try thread_pool.init(.{ .allocator = allocator });
    defer thread_pool.deinit();

    var bundler = try Bundler.init(allocator, &thread_pool);
    defer bundler.deinit();

    for (res.positionals) |pos| try bundler.bundle(stdout, pos);
}

test "renderer" {
    _ = @import("./renderer.zig");
}

test "test files" {
    _ = @import("./wgsl/test.zig");
}
