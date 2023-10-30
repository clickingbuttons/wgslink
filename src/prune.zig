const std = @import("std");
const Ast = @import("./shader/Ast.zig");

const Allocator = std.mem.Allocator;
const Node = Ast.Node;
const NodeIndex = Ast.NodeIndex;
const Root = struct {
    used: bool,
    node: NodeIndex,
};
const Roots = std.StringHashMap(Root);
const Self = @This();

allocator: Allocator,
tree: *Ast,
roots: Roots,

// Caller owns memory
fn findEntries(allocator: Allocator, ast: *Ast) ![]NodeIndex {
    var res = std.ArrayList(NodeIndex).init(allocator);
    for (ast.globals) |node| {
        if (ast.nodeTag(node) != .@"fn") continue;

        const extra = ast.extraData(Node.FnProto, ast.nodeLHS(node));
        if (extra.attrs == .none) continue;

        for (ast.spanToList(extra.attrs)) |a| {
            switch (ast.nodeTag(a)) {
                .attr_vertex,
                .attr_fragment,
                .attr_compute => {
                    try res.append(node);
                    break;
                },
                else => {},
            }
        }
    }

    return res.toOwnedSlice();
}

fn globalNameTree(tree: *Ast, node: NodeIndex) []const u8 {
    return tree.declNameLoc(node).?.slice(tree.source);
}

fn globalName(self: Self, node: NodeIndex) []const u8 {
    return globalNameTree(self.tree, node);
}

pub fn init(allocator: Allocator, tree: *Ast) !Self {
    var roots = Roots.init(allocator);
    for (tree.globals) |g| try roots.put(globalNameTree(tree, g), .{ .used = false, .node = g });
    return Self {
        .allocator = allocator,
        .tree = tree,
        .roots = roots,
    };
}

fn visitCall(self: *Self, node: NodeIndex) Allocator.Error!void {
    const ty = self.tree.nodeRHS(node);
    if (ty != .none) try self.visitType(ty);

    const args = self.tree.nodeLHS(node);
    if (args != .none) {
        for (self.tree.spanToList(args)) |arg| try self.visitExpr(arg);
    }
}

fn visitIf(self: *Self, node: NodeIndex) Allocator.Error!void {
    try self.visitExpr(self.tree.nodeLHS(node));
    try self.visitBlock(self.tree.nodeRHS(node));
}

fn visitVar(self: *Self, extra: anytype, node: NodeIndex) Allocator.Error!void {
    if (extra.type != .none) try self.visitType(extra.type);

    const rhs = self.tree.nodeRHS(node);
    if (rhs != .none) try self.visitExpr(rhs);
}

fn visitConst(self: *Self, node: NodeIndex) Allocator.Error!void {
    const lhs = self.tree.nodeLHS(node);
    const rhs = self.tree.nodeRHS(node);
    if (lhs != .none) try self.visitType(lhs);
    try self.visitExpr(rhs);
}

fn nodeName(self: *Self, node: NodeIndex) []const u8 {
    return self.tree.nodeLoc(node).slice(self.tree.source);
}

fn visitExpr(self: *Self, node: NodeIndex) Allocator.Error!void {
    const tag = self.tree.nodeTag(node);
    const lhs = self.tree.nodeLHS(node);
    const rhs = self.tree.nodeRHS(node);
    switch (tag) {
       .ident => {
            const name = self.tree.nodeLoc(node).slice(self.tree.source);
            if (self.roots.getPtr(name)) |r| {
                r.used = true;
                try self.visitGlobal(r.node);
            }
       },
       .number, .true, .false => {},
        .not, .negate, .deref, .addr_of, .paren_expr, .field_access => try self.visitExpr(lhs),
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
        => {
            try self.visitExpr(lhs);
            try self.visitExpr(rhs);
        },
        .index_access => {
            try self.visitExpr(lhs);
            try self.visitExpr(rhs);
        },
        .call => try self.visitCall(node),
        .bitcast => try self.visitExpr(rhs),
        else => |t| {
            std.debug.print("invalid expression {s}\n", .{@tagName(t)});
        },
    }
}

fn visitStatement(self: *Self, node: NodeIndex) Allocator.Error!bool {
    const tag = self.tree.nodeTag(node);
    const lhs = self.tree.nodeLHS(node);
    const rhs = self.tree.nodeRHS(node);

    switch (tag) {
        .compound_assign, .@"while" => {
            try self.visitExpr(lhs);
            try self.visitExpr(rhs);
        },
        .call => try self.visitCall(node),
        .@"return" => {
            if (lhs != .none) try self.visitExpr(lhs);
            return true;
        },
        .comment => {},
        .phony_assign => try self.visitExpr(lhs),
        .break_if => try self.visitExpr(lhs),
        .@"if" => try self.visitIf(node),
        .if_else => {
            try self.visitIf(lhs);
            try self.visitBlock(rhs);
        },
       .if_else_if => {
            try self.visitIf(lhs);
            try self.visitIf(rhs);
       },
        .@"for" => {
            const extra = self.tree.extraData(Node.ForHeader, self.tree.nodeLHS(node));
            if (extra.init != .none) _ = try self.visitStatement(extra.init);
            if (extra.cond != .none) _ = try self.visitExpr(extra.cond);
            if (extra.update != .none) _ = try self.visitStatement(extra.update);
            try self.visitBlock(rhs);
        },
      .@"switch" => {
            try self.visitExpr(lhs);
            if (rhs != .none) for (self.tree.spanToList(rhs)) |c| {
                switch (self.tree.nodeTag(c)) {
                    .switch_case => {
                        const expr_list = self.tree.nodeLHS(c);
                        if (expr_list != .none) {
                            const expressions = self.tree.spanToList(expr_list);
                            for (expressions) |e| try self.visitExpr(e);
                        }
                    },
                    else => {},
                }
                try self.visitBlock(self.tree.nodeRHS(c));
            };
        },
        .loop => {
            try self.visitBlock(lhs);
        },
        .block => try self.visitBlock(node),
        .continuing => try self.visitBlock(lhs),
        .discard, .@"break", .@"continue" => {},
        .increase, .decrease => try self.visitExpr(lhs),
        .@"const", .let => try self.visitConst(node),
        .@"var" => {
            const extra = self.tree.extraData(Node.Var, self.tree.nodeLHS(node));
            try self.visitVar(extra, node);
        },
        else => {
            std.debug.print("not visiting {s}\n", .{ @tagName(tag) });
        },
    }

    return false;
}

fn visitBlock(self: *Self, node: NodeIndex) Allocator.Error!void {
    const lhs = self.tree.nodeLHS(node);
    if (lhs != .none) {
        for (self.tree.spanToList(lhs)) |n| {
            if (try self.visitStatement(n)) break;
        }
    }
}

fn visitType(self: *Self, node: NodeIndex) Allocator.Error!void {
    if (self.tree.nodeTag(node) == .ident) {
        const name = self.tree.tokenLoc(self.tree.nodeToken(node)).slice(self.tree.source);
        if (self.roots.getPtr(name)) |r| {
            r.used = true;
            try self.visitGlobal(r.node);
        }
    }


    switch (self.tree.nodeTag(node)) {
        .array_type, .ptr_type => try self.visitType(self.tree.nodeLHS(node)),
        else => {},
    }
}

fn visitFn(self: *Self, node: NodeIndex) Allocator.Error!void {
    const extra = self.tree.extraData(Node.FnProto, self.tree.nodeLHS(node));
    if (extra.params != .none) {
        for (self.tree.spanToList(extra.params)) |p| try self.visitType(self.tree.nodeRHS(p));
    }
    if (extra.return_type != .none) try self.visitType(extra.return_type);

    try self.visitBlock(self.tree.nodeRHS(node));
}

fn visitGlobalVar(self: *Self, node: NodeIndex) Allocator.Error!void {
    const extra = self.tree.extraData(Node.GlobalVar, self.tree.nodeLHS(node));

    try self.visitVar(extra, node);
}

fn visitOverride(self: *Self, node: NodeIndex) Allocator.Error!void {
    const extra = self.tree.extraData(Node.Override, self.tree.nodeLHS(node));

    if (extra.type != .none) try self.visitType(extra.type);

    const rhs = self.tree.nodeRHS(node);
    if (rhs != .none) try self.visitExpr(rhs);
}

fn visitStruct(self: *Self, node: NodeIndex) Allocator.Error!void {
    const members = self.tree.spanToList(self.tree.nodeLHS(node));
    for (members) |n| try self.visitType(self.tree.nodeRHS(n));
}

fn visitGlobal(self: *Self, node: NodeIndex) Allocator.Error!void {
    switch (self.tree.nodeTag(node)) {
        .global_var => try self.visitGlobalVar(node),
        .override => try self.visitOverride(node),
        .@"const" => try self.visitConst(node),
        .@"struct" => try self.visitStruct(node),
        .@"fn" => try self.visitFn(node),
        // TODO: visit these first in case struct is aliased
        // .type_alias => try self.visitTypeAlias(r),
        .comment => {},
        else => |t| {
            std.debug.print("could not prune node {s}\n", .{@tagName(t)});
        },
    }
}

pub fn prune(self: *Self) Allocator.Error!void {
    var used_globals = try std.ArrayList(NodeIndex).initCapacity(self.allocator, self.tree.globals.len);
    errdefer used_globals.deinit();

    const entries = try findEntries(self.allocator, self.tree);
    defer self.allocator.free(entries);
    for (entries) |e| try self.visitGlobal(e);

    for (self.tree.globals) |g| {
        if (self.roots.get(self.globalName(g))) |u| {
            if (u.used) try used_globals.append(g);
        }
    }

    for (entries) |e| try used_globals.append(e);

    self.allocator.free(self.tree.globals);
    self.tree.globals = try used_globals.toOwnedSlice();
}
