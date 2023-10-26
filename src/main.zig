const std = @import("std");

const ErrorList = @import("./shader/ErrorList.zig");
const Ast = @import("./shader/Ast.zig");
const Air = @import("./shader/Air.zig");

const NodeIndex = Ast.NodeIndex;

pub fn main() !void {
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // try stdout.print("Run `zig build test` to run the tests.\n", .{});
    // try bw.flush();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var errors = try ErrorList.init(allocator);
    defer errors.deinit();

    const source =
        \\struct Strides {
        \\	instance: u32,
        \\};
        \\const mesh: u32 = 3;
        \\@group(mesh) @binding(0) var<uniform> strides: Strides;
        \\@group(mesh) @binding(1) var<storage> indices: array<u32>;
        \\@group(mesh) @binding(2) var<storage> positions: array<fp64>;
        \\
        \\@group(mesh) @binding(3) var<storage> inModel: array<fp64, 16>;
        \\@group(mesh) @binding(4) var<storage> models: array<array<fp64, 16>>;
        \\@group(mesh) @binding(5) var<storage> colors: array<u32>;
        \\@group(mesh) @binding(6) var<storage> instanceColors: array<u32>;
        \\@group(mesh) @binding(7) var<storage> normals: array<f32>;
    ;

    var tree = Ast.parse(allocator, &errors, source) catch |err| {
        if (err == error.Parsing) {
            try errors.print(source, null);
        }
        return err;
    };
    defer tree.deinit(allocator);

    const global_nodes = tree.spanToList(.globals);

    for (global_nodes) |node| {
        std.debug.print("{} {} {} {}\n", .{
            node,
            tree.nodeTag(node),
            tree.nodeLHS(node),
            tree.nodeRHS(node),
        });
    }

    var ir = Air.generate(allocator, &tree, &errors, null) catch |err| {
        if (err == error.AnalysisFail) {
            try errors.print(source, null);
        }
        return err;
    };
    defer ir.deinit(allocator);
}

test "wgsl compiles" {
    _ = @import("./shader/test.zig");
}
