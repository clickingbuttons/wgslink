const std = @import("std");

const ErrorList = @import("./shader/ErrorList.zig");
const Ast = @import("./shader/Ast.zig");

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
        \\// import { Foo } from './bar.wgsl';
        \\const a: vec2f = vec2f(1);
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
            @intFromEnum(node),
            tree.nodeTag(node),
            tree.nodeLHS(node),
            tree.nodeRHS(node),
        });
    }
    for (0..tree.nodes.len) |i| {
        const node: NodeIndex = @enumFromInt(i);
        const loc = tree.nodeLoc(node);
        std.debug.print("{} '{s}' {} {} {}\n", .{
            i,
            source[loc.start..loc.end],
            tree.nodeTag(node),
            tree.nodeLHS(node),
            tree.nodeRHS(node),
        });
    }
}

test "wgsl compiles" {
    _ = @import("./shader/test.zig");
}
