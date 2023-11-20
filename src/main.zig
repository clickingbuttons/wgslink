const std = @import("std");
const clap = @import("clap");
const Ast = @import("./ast/Ast.zig");
const Module = @import("./module.zig");
const Bundler = @import("./bundler.zig");

const ThreadPool = std.Thread.Pool;
const max_source = 1 << 30;
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

pub fn main() !void {
    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit.
        \\-m, --minify           Rename variables to be shorter and remove whitespace
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

    for (res.positionals) |pos| bundler.bundle(
        stdout,
        stderr,
        std.io.tty.detectConfig(std.io.getStdErr()),
        .{ .file = pos, .tree_shake = .{}, .minify = res.args.minify != 0 },
    ) catch |err| {
        stderr.print("error: {s} when bundling {s}\n", .{ @errorName(err), pos }) catch {};
        continue;
    };
}

test "renderer" {
    _ = @import("./wgsl/renderer.zig");
}

test "bundler" {
    _ = @import("./bundler.zig");
}
