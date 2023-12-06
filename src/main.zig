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
        \\-m, --minify           Remove whitespace.
        \\-e, --entry <str>...   Symbols in entry files' global scope to NOT tree shake.
        \\
        \\	If not specified will default to functions with @vertex, @fragment, or @compute attributes.
        \\
        \\	If any entry is "*" no tree shaking will occur.
        \\<str>...               Entry files
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

    var tree_shake = true;
    for (res.args.entry) |e| {
        if (e.len == 1 and e[0] == '*') tree_shake = false;
    }
    const opts = Bundler.Options{
        .entrypoints = if (tree_shake) res.args.entry else null,
        .minify = res.args.minify != 0,
    };
    var bundler = try Bundler.init(allocator, &thread_pool, opts);
    defer bundler.deinit();

    var errconfig = std.io.tty.detectConfig(std.io.getStdErr());

    var failed = false;
    for (res.positionals) |pos| {
        bundler.bundle(stdout, stderr, errconfig, pos) catch |err| {
            errconfig.setColor(stderr, .red) catch {};
            switch (err) {
                error.UnparsedModule => {},
                else => {
                    stderr.print("error: {s} when bundling {s}\n", .{ @errorName(err), pos }) catch {};
                },
            }
            errconfig.setColor(stderr, .reset) catch {};
            failed = true;
            continue;
        };
    }

    if (failed) std.os.exit(1);
}

test "bundler" {
    _ = @import("./bundler.zig");
}
