const std = @import("std");
const Ast = @import("./wgsl/Ast.zig");
const Node = @import("./wgsl/Node.zig");

const Allocator = std.mem.Allocator;
const Used = std.StringHashMap(void);

fn findSymbols(used: *Used, tree: *Ast) !void {
    for (tree.spanToList(0)) |node| {
        if (tree.nodeTag(node) != .@"fn") continue;

        const extra = tree.extraData(Node.FnProto, tree.nodeLHS(node));
        if (extra.attrs == 0) continue;

        for (tree.spanToList(extra.attrs)) |a| {
            switch (tree.nodeTag(a)) {
                .attr_vertex, .attr_fragment, .attr_compute => {
                    try used.put(tree.declNameSource(node), {});
                    break;
                },
                else => {},
            }
        }
    }
}

fn addUsedSymbol(tree: *Ast, used: *Used, symbol: []const u8) Allocator.Error!void {
    if (used.get(symbol)) |_| return;
    try used.put(symbol, {});
    for (tree.spanToList(0)) |node| {
        switch (tree.nodeTag(node)) {
            .global_var, .override, .@"const", .@"struct", .@"fn", .type_alias => {
                if (std.mem.eql(u8, tree.declNameSource(node), symbol)) try visitGlobal(tree, used, node);
            },
            else => {},
        }
    }
}

fn visitCall(tree: *Ast, used: *Used, node: Node.Index) Allocator.Error!void {
    try visitType(tree, used, tree.nodeLHS(node));

    const args = tree.nodeRHS(node);
    if (args != 0) {
        for (tree.spanToList(args)) |arg| try visitExpr(tree, used, arg);
    }
}

fn visitIf(tree: *Ast, used: *Used, node: Node.Index) Allocator.Error!void {
    try visitExpr(tree, used, tree.nodeLHS(node));
    try visitCompoundStatement(tree, used, tree.nodeRHS(node));
}

fn visitVar(tree: *Ast, used: *Used, extra: anytype, node: Node.Index) Allocator.Error!void {
    if (extra.type != 0) try visitType(tree, used, extra.type);

    const rhs = tree.nodeRHS(node);
    try visitExpr(tree, used, rhs);
}

fn visitConst(tree: *Ast, used: *Used, node: Node.Index) Allocator.Error!void {
    const lhs = tree.nodeLHS(node);
    const rhs = tree.nodeRHS(node);
    try visitType(tree, used, lhs);
    try visitExpr(tree, used, rhs);
}

fn visitExpr(tree: *Ast, used: *Used, node: Node.Index) Allocator.Error!void {
    if (node == 0) return;
    const lhs = tree.nodeLHS(node);
    const rhs = tree.nodeRHS(node);
    switch (tree.nodeTag(node)) {
        .ident => try addUsedSymbol(tree, used, tree.nodeSource(node)),
        .number, .true, .false => {},
        .not, .negate, .deref, .addr_of, .paren_expr, .field_access => try visitExpr(tree, used, lhs),
        .mul,
        .div,
        .mod,
        .add,
        .sub,
        .shl,
        .shr,
        .@"and",
        .@"or",
        .xor,
        .logical_and,
        .logical_or,
        .equal,
        .not_equal,
        .less_than,
        .less_than_equal,
        .greater_than,
        .greater_than_equal,
        .index_access,
        => {
            try visitExpr(tree, used, lhs);
            try visitExpr(tree, used, rhs);
        },
        .call => try visitCall(tree, used, node),
        else => |t| {
            std.debug.print("invalid expression {s}\n", .{@tagName(t)});
        },
    }
}

fn visitStatement(tree: *Ast, used: *Used, node: Node.Index) Allocator.Error!void {
    const lhs = tree.nodeLHS(node);
    const rhs = tree.nodeRHS(node);

    switch (tree.nodeTag(node)) {
        .compound_assign => {
            try visitExpr(tree, used, lhs);
            try visitExpr(tree, used, rhs);
        },
        .phony_assign => try visitExpr(tree, used, lhs),
        .call => try visitCall(tree, used, node),
        .@"return" => try visitExpr(tree, used, lhs),
        .comment => {},
        .break_if => try visitExpr(tree, used, lhs),
        .@"if" => try visitIf(tree, used, node),
        .else_if => {
            try visitIf(tree, used, lhs);
            try visitStatement(tree, used, rhs);
        },
        .@"else" => {
            try visitIf(tree, used, lhs);
            try visitCompoundStatement(tree, used, rhs);
        },
        .@"while" => {
            try visitExpr(tree, used, lhs);
            try visitCompoundStatement(tree, used, rhs);
        },
        .@"for" => {
            const extra = tree.extraData(Node.ForHeader, tree.nodeLHS(node));
            try visitStatement(tree, used, extra.init);
            try visitExpr(tree, used, extra.cond);
            try visitStatement(tree, used, extra.update);
            try visitCompoundStatement(tree, used, rhs);
        },
        .@"switch" => {
            try visitExpr(tree, used, lhs);
            try visitSwitchBody(tree, used, rhs);
        },
        .loop => try visitCompoundStatement(tree, used, rhs),
        .compound_statement => try visitCompoundStatement(tree, used, node),
        .continuing => try visitCompoundStatement(tree, used, lhs),
        .discard, .@"break", .@"continue" => {},
        .increase, .decrease => try visitExpr(tree, used, lhs),
        .@"const", .let => try visitConst(tree, used, node),
        .@"var" => {
            const extra = tree.extraData(Node.Var, tree.nodeLHS(node));
            try visitVar(tree, used, extra, node);
        },
        else => |t| {
            std.debug.print("not visiting {s}\n", .{@tagName(t)});
        },
    }
}

fn visitCaseSelector(tree: *Ast, used: *Used, node: Node.Index) !void {
    try visitExpr(tree, used, tree.nodeLHS(node));
}

fn visitSwitchClause(tree: *Ast, used: *Used, node: Node.Index) !void {
    const selectors = tree.nodeLHS(node);
    if (selectors != 0) {
        const list = tree.spanToList(selectors);
        for (list) |s| try visitCaseSelector(tree, used, s);
    }
    try visitCompoundStatement(tree, used, tree.nodeRHS(node));
}

fn visitSwitchBody(tree: *Ast, used: *Used, node: Node.Index) !void {
    for (tree.spanToList(tree.nodeRHS(node))) |c| try visitSwitchClause(tree, used, c);
}

fn visitCompoundStatement(tree: *Ast, used: *Used, node: Node.Index) Allocator.Error!void {
    const rhs = tree.nodeRHS(node);
    if (rhs != 0) {
        for (tree.spanToList(rhs)) |n| try visitStatement(tree, used, n);
    }
}

fn visitTemplateElaboratedIdent(tree: *Ast, used: *Used, node: Node.Index) !void {
    if (node == 0) return;
    try addUsedSymbol(tree, used, tree.nodeSource(node));
    const lhs = tree.nodeLHS(node);
    if (lhs != 0) {
        for (tree.spanToList(lhs)) |n| try visitExpr(tree, used, n);
    }
}

fn visitType(tree: *Ast, used: *Used, node: Node.Index) Allocator.Error!void {
    try visitTemplateElaboratedIdent(tree, used, node);
}

fn visitFn(tree: *Ast, used: *Used, node: Node.Index) Allocator.Error!void {
    const extra = tree.extraData(Node.FnProto, tree.nodeLHS(node));
    if (extra.params != 0) {
        for (tree.spanToList(extra.params)) |p| try visitType(tree, used, tree.nodeRHS(p));
    }
    if (extra.return_type != 0) try visitType(tree, used, extra.return_type);

    try addUsedSymbol(tree, used, tree.declNameSource(node));
    try visitCompoundStatement(tree, used, tree.nodeRHS(node));
}

fn visitGlobalVar(tree: *Ast, used: *Used, node: Node.Index) Allocator.Error!void {
    const extra = tree.extraData(Node.GlobalVar, tree.nodeLHS(node));

    try visitVar(tree, used, extra, node);
}

fn visitOverride(tree: *Ast, used: *Used, node: Node.Index) Allocator.Error!void {
    const extra = tree.extraData(Node.Override, tree.nodeLHS(node));

    if (extra.type != 0) try visitType(tree, used, extra.type);

    const rhs = tree.nodeRHS(node);
    if (rhs != 0) try visitExpr(tree, used, rhs);
}

fn visitStruct(tree: *Ast, used: *Used, node: Node.Index) Allocator.Error!void {
    const members = tree.spanToList(tree.nodeLHS(node));
    for (members) |n| try visitType(tree, used, tree.nodeRHS(n));
}

fn visitTypeAlias(tree: *Ast, used: *Used, node: Node.Index) Allocator.Error!void {
    try visitType(tree, used, tree.nodeLHS(node));
}

fn visitConstAssert(tree: *Ast, used: *Used, node: Node.Index) Allocator.Error!void {
    try visitExpr(tree, used, tree.nodeLHS(node));
}

fn visitGlobal(tree: *Ast, used: *Used, node: Node.Index) Allocator.Error!void {
    switch (tree.nodeTag(node)) {
        .global_var => try visitGlobalVar(tree, used, node),
        .override => try visitOverride(tree, used, node),
        .@"const" => try visitConst(tree, used, node),
        .const_assert => try visitConstAssert(tree, used, node),
        .type_alias => try visitTypeAlias(tree, used, node),
        .@"struct" => try visitStruct(tree, used, node),
        .@"fn" => try visitFn(tree, used, node),
        .comment => {},
        else => |t| {
            std.debug.print("could not prune node {s}\n", .{@tagName(t)});
        },
    }
}

pub const Options = struct {
    symbols: [][]const u8 = &.{},
    find_symbols: bool = true,
};

/// Tree shakes globals by setting unused root span nodes to empty.
/// Sets unused import nodes to empty.
pub fn treeShake(tree: *Ast, allocator: Allocator, opts: Options) Allocator.Error!void {
    var used = Used.init(allocator);
    defer used.deinit();
    for (opts.symbols) |s| try used.put(s, {});
    if (opts.find_symbols) try findSymbols(&used, tree);

    // Find all used identifiers
    for (tree.spanToList(0)) |node| {
        switch (tree.nodeTag(node)) {
            .global_var, .override, .@"const", .@"struct", .@"fn", .type_alias => {
                if (used.get(tree.declNameSource(node))) |_| try visitGlobal(tree, &used, node);
            },
            else => {},
        }
    }

    for (tree.spanToList(0)) |node| {
        switch (tree.nodeTag(node)) {
            // Set unused root span nodes to empty.
            .global_var, .override, .@"const", .@"struct", .@"fn", .type_alias => {
                if (used.get(tree.declNameSource(node)) == null) tree.nodes.items(.tag)[node] = .empty;
            },
            // Set unused import nodes to empty.
            .import => {
                var import_used = false;
                const lhs = tree.nodeLHS(node);
                if (lhs != 0) {
                    for (tree.spanToList(lhs)) |n| {
                        if (used.get(tree.nodeSource(n))) |_| import_used = true else tree.nodes.items(.tag)[n] = .empty;
                    }
                }
                if (!import_used) tree.nodes.items(.tag)[node] = .empty;
            },
            else => {},
        }
    }
}
