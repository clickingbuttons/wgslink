const std = @import("std");
const clap = @import("clap");
const Ast = @import("./ast/Ast.zig");
const Module = @import("./module.zig");
const Bundler = @import("./bundler.zig");
const renderer = @import("./WgslRenderer.zig").renderer;
const Layouts = @import("./layouts.zig");

const Allocator = std.mem.Allocator;
const ThreadPool = std.Thread.Pool;
const max_source = 1 << 30;
var failed = false;

fn fail(comptime fmt: []const u8, args: anytype) void {
    const stderr = std.io.getStdErr();
    const errconfig = std.io.tty.detectConfig(stderr);
    errconfig.setColor(stderr, .red) catch {};
    std.debug.print(fmt, args);
    errconfig.setColor(stderr, .reset) catch {};
    failed = true;
}

fn createFile(allocator: Allocator, dir: []const u8, path: []const u8, ext: []const u8) !std.fs.File {
    const basename = try std.fmt.allocPrint(allocator, "{s}{s}", .{
        std.fs.path.basename(path),
        ext,
    });
    defer allocator.free(basename);
    const fname = try std.fs.path.join(allocator, &.{ dir, basename });
    defer allocator.free(fname);
    return std.fs.cwd().createFile(fname, .{}) catch |err| {
        fail("{} when creating {s}\n", .{ err, fname });
        return err;
    };
}

fn bundleAndWrite(args: anytype, bundler: *Bundler, fname: []const u8) !void {
    const allocator = bundler.allocator;
    const stderr = std.io.getStdErr();
    const errconfig = std.io.tty.detectConfig(stderr);

    var tree = bundler.bundle(stderr.writer(), errconfig, fname) catch |err| {
        failed = true;
        switch (err) {
            error.UnparsedModule => return,
            else => {
                fail("{} when bundling {s}\n", .{ err, fname });
                return err;
            },
        }
    };
    defer tree.deinit(allocator);

    const outfile = if (args.outdir) |o| try createFile(allocator, o, fname, "") else std.io.getStdOut();
    defer if (args.outdir) |_| outfile.close();

    var wgsl = renderer(outfile.writer(), .{
        .minify = bundler.opts.minify,
        .render_imports = false,
    });
    try wgsl.writeTranslationUnit(tree);
    // be nice to ttys
    if (args.outdir == null) try outfile.writer().writeByte('\n');

    if (args.layout != 0) {
        const attribfile = if (args.outdir) |o| try createFile(allocator, o, fname, ".json") else stderr;
        defer if (args.outdir) |_| attribfile.close();

        var layouts = Layouts.init(allocator, tree) catch |err| {
            fail("{} when getting layout for {s}\n", .{ err, fname });
            return;
        };
        defer layouts.deinit();

        const options = std.json.StringifyOptions{
            .emit_null_optional_fields = false,
            .whitespace = .indent_tab,
        };
        try std.json.stringify(layouts, options, attribfile.writer());

        // be nice to ttys
        if (args.outdir == null) try attribfile.writer().writeByte('\n');
    }
}

pub fn main() !void {
    const stderr = std.io.getStdErr();
    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit.
        \\-m, --minify           Remove whitespace.
        \\-l, --layout           Extract bind group and struct layouts to JSON file or stderr.
        \\-o, --outdir <str>     Output directory (otherwise will print to stdout).
        \\-e, --entry <str>...   Symbols in entry files' global scope to NOT tree shake.
        \\
        \\	If not specified will default to functions with @vertex, @fragment, or @compute attributes.
        \\
        \\	If any entry is "*" no tree shaking will occur.
        \\<str>...               Entry files
        \\
    );
    var diag = clap.Diagnostic{};
    var parsed = clap.parse(
        clap.Help,
        &params,
        clap.parsers.default,
        .{ .diagnostic = &diag },
    ) catch |err| {
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
        .tree_shake = tree_shake,
        .entrypoints = args.entry,
        .minify = args.minify != 0,
    };
    var bundler = try Bundler.init(allocator, &thread_pool, opts);
    defer bundler.deinit();

    if (args.outdir) |o| std.fs.cwd().makePath(o) catch |err| {
        fail("{} when makePath {s}\n", .{ err, o });
        return err;
    };

    for (parsed.positionals) |fname| bundleAndWrite(args, &bundler, fname) catch {};

    std.os.exit(@intFromBool(failed));
}

test "bundler" {
    _ = @import("./bundler.zig");
    _ = @import("./WgslRenderer.zig");
}
