const std = @import("std");
const clap = @import("clap");
const Ast = @import("./ast/Ast.zig");
const Module = @import("./module.zig");
const Bundler = @import("./bundler.zig");

const ThreadPool = std.Thread.Pool;
const max_source = 1 << 30;
const stdout = std.io.getStdOut();
const stderr = std.io.getStdErr();
var failed = false;

pub fn fail(comptime fmt: []const u8, args: anytype, errconfig: std.io.tty.Config) void {
    errconfig.setColor(stderr, .red) catch {};
    std.debug.print(fmt, args);
    errconfig.setColor(stderr, .reset) catch {};
    failed = true;
}

pub fn main() !void {
    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit.
        \\-m, --minify           Remove whitespace.
        \\-o, --outdir <str>     Output directory (otherwise will print to stdout)
        \\-e, --entry <str>...   Symbols in entry files' global scope to NOT tree shake.
        \\
        \\	If not specified will default to functions with @vertex, @fragment, or @compute attributes.
        \\
        \\	If any entry is "*" no tree shaking will occur.
        \\<str>...               Entry files
        \\
    );
    var diag = clap.Diagnostic{};
    var parsed = clap.parse(clap.Help, &params, clap.parsers.default, .{ .diagnostic = &diag }) catch |err| {
        diag.report(stderr.writer(), err) catch {};
        return err;
    };
    defer parsed.deinit();

    const args = parsed.args;
    if (args.help != 0 or parsed.positionals.len == 0)
        return clap.help(stderr.writer(), clap.Help, &params, .{});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var thread_pool: ThreadPool = undefined;
    try thread_pool.init(.{ .allocator = allocator });
    defer thread_pool.deinit();

    var tree_shake = true;
    for (args.entry) |e| {
        if (e.len == 1 and e[0] == '*') tree_shake = false;
    }
    const opts = Bundler.Options{
        .entrypoints = if (tree_shake) args.entry else null,
        .minify = args.minify != 0,
    };
    var bundler = try Bundler.init(allocator, &thread_pool, opts);
    defer bundler.deinit();

    const errconfig = std.io.tty.detectConfig(stderr);
    if (args.outdir) |o| std.fs.cwd().makePath(o) catch |err| {
        fail("{} when makePath {s}\n", .{ err, o }, errconfig);
        return err;
    };

    for (parsed.positionals) |fname| {
        const outfile = if (args.outdir) |o| brk: {
            const basename = std.fs.path.basename(fname);
            const path = try std.fs.path.join(allocator, &.{ o, basename });
            defer allocator.free(path);
            var file = std.fs.cwd().createFile(path, .{}) catch |err| {
                fail("{} when creating {s}\n", .{ err, path }, errconfig);
                continue;
            };
            break :brk file;
        } else std.io.getStdOut();
        defer if (args.outdir) |_| outfile.close();

        bundler.bundle(outfile.writer(), stderr.writer(), errconfig, fname) catch |err| switch (err) {
            error.UnparsedModule => {},
            else => fail("{} when bundling {s}\n", .{ err, fname }, errconfig),
        };
    }

    if (failed) std.os.exit(1);
}

test "bundler" {
    _ = @import("./bundler.zig");
}
