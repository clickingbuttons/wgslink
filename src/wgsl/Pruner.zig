const std = @import("std");
const Ast = @import("./Ast.zig");

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
roots: Roots,

// Caller owns memory
fn findEntryFns(self: Self, ast: *Ast) ![]NodeIndex {
    var res = std.ArrayList(NodeIndex).init(self.allocator);
    for (ast.globals) |node| {
        if (ast.nodeTag(node) != .@"fn") continue;

        const extra = ast.extraData(Node.FnProto, ast.nodeLHS(node));
        if (extra.attrs == .none) continue;

        for (ast.spanToList(extra.attrs)) |a| {
            switch (ast.nodeTag(a)) {
                .attr_vertex, .attr_fragment, .attr_compute => {
                    try res.append(node);
                    break;
                },
                else => {},
            }
        }
    }

    return res.toOwnedSlice();
}

pub fn init(allocator: Allocator) !Self {
    var roots = Roots.init(allocator);
    return Self{
        .allocator = allocator,
        .roots = roots,
    };
}

pub fn deinit(self: *Self) void {
    self.roots.deinit();
    self.* = undefined;
}

fn visitCall(self: *Self, ast: *Ast, node: NodeIndex) Allocator.Error!void {
    const ty = ast.nodeRHS(node);
    if (ty != .none) try self.visitType(ast, ty);

    const args = ast.nodeLHS(node);
    if (args != .none) {
        for (ast.spanToList(args)) |arg| try self.visitExpr(ast, arg);
    }
}

fn visitIf(self: *Self, ast: *Ast, node: NodeIndex) Allocator.Error!void {
    try self.visitExpr(ast, ast.nodeLHS(node));
    try self.visitBlock(ast, ast.nodeRHS(node));
}

fn visitVar(self: *Self, ast: *Ast, extra: anytype, node: NodeIndex) Allocator.Error!void {
    if (extra.type != .none) try self.visitType(ast, extra.type);

    const rhs = ast.nodeRHS(node);
    if (rhs != .none) try self.visitExpr(ast, rhs);
}

fn visitConst(self: *Self, ast: *Ast, node: NodeIndex) Allocator.Error!void {
    const lhs = ast.nodeLHS(node);
    const rhs = ast.nodeRHS(node);
    if (lhs != .none) try self.visitType(ast, lhs);
    try self.visitExpr(ast, rhs);
}

fn visitExpr(self: *Self, ast: *Ast, node: NodeIndex) Allocator.Error!void {
    const tag = ast.nodeTag(node);
    const lhs = ast.nodeLHS(node);
    const rhs = ast.nodeRHS(node);
    switch (tag) {
        .ident => {
            const name = ast.nodeLoc(node).slice(ast.source);
            if (self.roots.getPtr(name)) |r| {
                r.used = true;
                try self.visitGlobal(ast, r.node);
            }
        },
        .number, .true, .false => {},
        .not, .negate, .deref, .addr_of, .paren_expr, .field_access => try self.visitExpr(ast, lhs),
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
            try self.visitExpr(ast, lhs);
            try self.visitExpr(ast, rhs);
        },
        .index_access => {
            try self.visitExpr(ast, lhs);
            try self.visitExpr(ast, rhs);
        },
        .call => try self.visitCall(ast, node),
        .bitcast => try self.visitExpr(ast, rhs),
        else => |t| {
            std.debug.print("invalid expression {s}\n", .{@tagName(t)});
        },
    }
}

fn visitStatement(self: *Self, ast: *Ast, node: NodeIndex) Allocator.Error!bool {
    const tag = ast.nodeTag(node);
    const lhs = ast.nodeLHS(node);
    const rhs = ast.nodeRHS(node);

    switch (tag) {
        .compound_assign, .@"while" => {
            try self.visitExpr(ast, lhs);
            try self.visitExpr(ast, rhs);
        },
        .call => try self.visitCall(ast, node),
        .@"return" => {
            if (lhs != .none) try self.visitExpr(ast, lhs);
            return true;
        },
        .comment => {},
        .phony_assign, .break_if => try self.visitExpr(ast, lhs),
        .@"if" => try self.visitIf(ast, node),
        .if_else => {
            try self.visitIf(ast, lhs);
            try self.visitBlock(ast, rhs);
        },
        .if_else_if => {
            try self.visitIf(ast, lhs);
            try self.visitIf(ast, rhs);
        },
        .@"for" => {
            const extra = ast.extraData(Node.ForHeader, ast.nodeLHS(node));
            if (extra.init != .none) _ = try self.visitStatement(ast, extra.init);
            if (extra.cond != .none) _ = try self.visitExpr(ast, extra.cond);
            if (extra.update != .none) _ = try self.visitStatement(ast, extra.update);
            try self.visitBlock(ast, rhs);
        },
        .@"switch" => {
            try self.visitExpr(ast, lhs);
            if (rhs != .none) for (ast.spanToList(rhs)) |c| {
                switch (ast.nodeTag(c)) {
                    .switch_case => {
                        const expr_list = ast.nodeLHS(c);
                        if (expr_list != .none) {
                            const expressions = ast.spanToList(expr_list);
                            for (expressions) |e| try self.visitExpr(ast, e);
                        }
                    },
                    else => {},
                }
                try self.visitBlock(ast, ast.nodeRHS(c));
            };
        },
        .block => try self.visitBlock(ast, node),
        .loop, .continuing => try self.visitBlock(ast, lhs),
        .discard, .@"break", .@"continue" => {},
        .increase, .decrease => try self.visitExpr(ast, lhs),
        .@"const", .let => try self.visitConst(ast, node),
        .@"var" => {
            const extra = ast.extraData(Node.Var, ast.nodeLHS(node));
            try self.visitVar(ast, extra, node);
        },
        else => {
            std.debug.print("not visiting {s}\n", .{@tagName(tag)});
        },
    }

    return false;
}

fn visitBlock(self: *Self, ast: *Ast, node: NodeIndex) Allocator.Error!void {
    const lhs = ast.nodeLHS(node);
    if (lhs != .none) {
        for (ast.spanToList(lhs)) |n| {
            if (try self.visitStatement(ast, n)) break;
        }
    }
}

fn visitType(self: *Self, ast: *Ast, node: NodeIndex) Allocator.Error!void {
    if (ast.nodeTag(node) == .ident) {
        const name = ast.tokenLoc(ast.nodeToken(node)).slice(ast.source);
        if (self.roots.getPtr(name)) |r| {
            r.used = true;
            try self.visitGlobal(ast, r.node);
        }
    }

    switch (ast.nodeTag(node)) {
        .array_type, .ptr_type => try self.visitType(ast, ast.nodeLHS(node)),
        else => {},
    }
}

fn visitFn(self: *Self, ast: *Ast, node: NodeIndex) Allocator.Error!void {
    const extra = ast.extraData(Node.FnProto, ast.nodeLHS(node));
    if (extra.params != .none) {
        for (ast.spanToList(extra.params)) |p| try self.visitType(ast, ast.nodeRHS(p));
    }
    if (extra.return_type != .none) try self.visitType(ast, extra.return_type);

    try self.visitBlock(ast, ast.nodeRHS(node));
}

fn visitGlobalVar(self: *Self, ast: *Ast, node: NodeIndex) Allocator.Error!void {
    const extra = ast.extraData(Node.GlobalVar, ast.nodeLHS(node));

    try self.visitVar(ast, extra, node);
}

fn visitOverride(self: *Self, ast: *Ast, node: NodeIndex) Allocator.Error!void {
    const extra = ast.extraData(Node.Override, ast.nodeLHS(node));

    if (extra.type != .none) try self.visitType(ast, extra.type);

    const rhs = ast.nodeRHS(node);
    if (rhs != .none) try self.visitExpr(ast, rhs);
}

fn visitStruct(self: *Self, ast: *Ast, node: NodeIndex) Allocator.Error!void {
    const members = ast.spanToList(ast.nodeLHS(node));
    for (members) |n| try self.visitType(ast, ast.nodeRHS(n));
}

fn visitGlobal(self: *Self, ast: *Ast, node: NodeIndex) Allocator.Error!void {
    switch (ast.nodeTag(node)) {
        .global_var => try self.visitGlobalVar(ast, node),
        .override => try self.visitOverride(ast, node),
        .@"const" => try self.visitConst(ast, node),
        .@"struct" => try self.visitStruct(ast, node),
        .@"fn" => try self.visitFn(ast, node),
        // TODO: visit these first in case struct is aliased
        // .type_alias => try self.visitTypeAlias(ast, r),
        .comment => {},
        else => |t| {
            std.debug.print("could not prune node {s}\n", .{@tagName(t)});
        },
    }
}

pub fn prune(self: *Self, ast: *Ast) Allocator.Error!void {
    self.roots.clearRetainingCapacity();
    for (ast.globals) |g| try self.roots.put(ast.globalName(g), .{ .used = false, .node = g });

    var used_globals = try std.ArrayList(NodeIndex).initCapacity(ast.allocator, ast.globals.len);
    errdefer used_globals.deinit();

    const entryFns = try self.findEntryFns(ast);
    defer self.allocator.free(entryFns);
    for (entryFns) |e| try self.visitGlobal(ast, e);

    for (ast.globals) |g| {
        if (self.roots.get(ast.globalName(g))) |u| {
            if (u.used) try used_globals.append(g);
        }
    }

    for (entryFns) |e| try used_globals.append(e);

    ast.allocator.free(ast.globals);
    ast.globals = try used_globals.toOwnedSlice();
}
