const std = @import("std");
const clap = @import("clap");
const Ast = @import("./wgsl/Ast.zig");
const Renderer = @import("./renderer.zig");
// const Pruner = @import("./wgsl/Pruner.zig");

const NodeIndex = Ast.NodeIndex;
const maxSourceSize = 1 << 30;

fn writeFormatted(
    allocator: std.mem.Allocator,
    writer: anytype,
    source: [:0]const u8,
    prune: bool,
) !void {
    var ast = try Ast.init(allocator, source);
    defer ast.deinit(allocator);

    _ = prune;
    // if (prune) {
    //     var pruner = try Pruner.init(allocator);
    //     defer pruner.deinit();
    //     try pruner.prune(&ast);
    // }

    try Renderer.writeTranslationUnit(&ast, "  ", writer);
}

pub fn main() !void {
    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit.
        \\<str>...
        \\
    );
    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{ .diagnostic = &diag }) catch |err| {
        diag.report(std.io.getStdErr().writer(), err) catch {};
        return err;
    };
    defer res.deinit();

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    for (res.positionals) |pos| {
        const file = try std.fs.cwd().openFile(pos, .{});
        defer file.close();

        const source = try file.readToEndAllocOptions(allocator, maxSourceSize, null, @alignOf(u8), 0);
        defer allocator.free(source);

        try writeFormatted(allocator, stdout, source, true);
    }
    if (res.positionals.len == 0) {
        const source = try std.io.getStdIn().readToEndAllocOptions(allocator, maxSourceSize, null, @alignOf(u8), 0);
        defer allocator.free(source);

        try writeFormatted(allocator, stdout, source, true);
    }

    try bw.flush();
}

test "renderer" {
    _ = @import("./renderer.zig");
}

test "test files" {
    _ = @import("./wgsl/test.zig");
}
