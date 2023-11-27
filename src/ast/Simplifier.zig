const Ast = @import("./Ast.zig");
const node_mod = @import("./Node.zig");

const Node = node_mod.Node;

pub fn simplify(tree: *Ast) void {
    // simplifyMath(tree);
    simplifyConditions(tree);
}

fn visitIndex(tree: *Ast, index: Node.Index) void {
    if (index == 0) return;
    visitNode(tree, tree.node(index));
}

fn visitNode(tree: *Ast, node: Node) void {
    switch (node) {
        .span => |n| {
            for (tree.extraData[n.from..n.to]) |i| visitIndex(tree, i);
        },
        .@"fn" => |n| visitIndex(tree, n.body),
        .compound => |n| {
            const indices = tree.spanToList(if (n.statements == 0) null else n.statements);
            for (indices) |i| visitNode(tree, tree.node(i));
        },
        .@"if" => |*n| {
            switch (tree.node(n.condition)) {
                .paren => |p| n.condition = p.expr,
                else => {},
            }
            visitIndex(tree, n.body);
        },
        .else_if => |n| {
            visitIndex(tree, n.if1);
            visitIndex(tree, n.if2);
        },
        .@"else" => |n| {
            visitIndex(tree, n.@"if");
            visitIndex(tree, n.body);
        },
        else => {},
    }
}

fn simplifyConditions(tree: *Ast) void {
    visitNode(tree.node(0));
}

// const std = @import("std");
// const Parser = @import("../wgsl/Parser.zig");
//
// fn testAst() !void {
//     const allocator = std.testing.allocator;
//     var parser = try Parser.init(allocator, source);
//     defer parser.deinit();
//
//     if (parser.errors.items.len > 0) {
//         const stderr = std.io.getStdErr();
//         const term = std.io.tty.detectConfig(stderr);
//         try stderr.writer().writeByte('\n');
//         try parser.renderErrors(stderr.writer(), term, null);
//     } else {
//         var tree = try parser.toOwnedAst();
//         defer tree.deinit(allocator);
//
//         var renderer = Renderer(@TypeOf(writer)).init(writer, minify);
//         try renderer.writeTranslationUnit(tree);
//     }
// }
//
// test "simplify if statements" {
//     try testRender(
//         "if (foo) {}",
//         "if foo {}",
//         true,
//     );
// }
