const std = @import("std");
const Ast = @import("./wgsl/Ast.zig");
const Node = @import("./wgsl/Node.zig");
const Token = @import("./wgsl/Token.zig");

const Self = @This();
const Allocator = std.mem.Allocator;
// { [scope key]: [global alias] }
const Scope = std.StringArrayHashMap([]const u8);
const Scopes = std.ArrayList(Scope);
const Diagnostic = struct {
    severity: Node.Severity,
    rule: []const u8,
};
const Directives = struct {
    const StringSet = std.StringArrayHashMap(void);
    const Diagnostics = std.AutoArrayHashMap(Diagnostic, void);
    enables: StringSet,
    language: StringSet,
    diagnostics: Diagnostics,

    pub fn init(allocator: Allocator) Directives {
        return Directives{
            .enables = StringSet.init(allocator),
            .language = StringSet.init(allocator),
            .diagnostics = Diagnostics.init(allocator),
        };
    }

    pub fn deinit(self: *Directives) void {
        self.enables.deinit();
        self.language.deinit();
        self.diagnostics.deinit();
    }
};

allocator: Allocator,
scopes: Scopes,
minify: bool,

source: std.ArrayList(u8),
directives: Directives,
tokens: Ast.TokenList = .{},
nodes: Ast.NodeList = .{},
extra: std.ArrayListUnmanaged(Node.Index) = .{},

pub fn init(allocator: Allocator, minify: bool) !Self {
    var scopes = Scopes.init(allocator);
    try scopes.append(Scope.init(allocator));
    return Self{
        .allocator = allocator,
        .scopes = scopes,
        .minify = minify,
        .source = std.ArrayList(u8).init(allocator),
        .directives = Directives.init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    for (self.scopes.items) |*s| {
        for (s.values()) |v| self.allocator.free(v);
        s.deinit();
    }
    self.scopes.deinit();
    self.directives.deinit();
}

fn addNode(self: *Self, node: Node) error{OutOfMemory}!Node.Index {
    const i: Node.Index = @intCast(self.nodes.len);
    try self.nodes.append(self.allocator, node);
    return i;
}

fn addExtra(self: *Self, extra: anytype) error{OutOfMemory}!Node.Index {
    const fields = std.meta.fields(@TypeOf(extra));
    try self.extra.ensureUnusedCapacity(self.allocator, fields.len);
    const result: Node.Index = @intCast(self.extra.items.len);
    inline for (fields) |field| {
        comptime std.debug.assert(field.type == Node.Index or field.type == Token.Index);
        self.extra.appendAssumeCapacity(@field(extra, field.name));
    }
    return result;
}

fn getOrAddGlobalAlias(self: *Self, ident: []const u8) !Node.Index {
    const scope_index = self.scopes.items.len - 1;
    var scope: *Scope = &self.scopes.items[scope_index];
    const gop = try scope.getOrPut(ident);
    if (!gop.found_existing) {
        // Make new unique identifier
        const count = brk: {
            var res: Node.Index = 1;
            for (self.scopes.items) |s| {
                if (s.get(ident) != null) res += 1;
            }
            break :brk res;
        };
        gop.value_ptr.* = if (count == 1)
            try self.allocator.dupe(u8, ident)
        else
            try std.fmt.allocPrint(self.allocator, "{s}{d}", .{ ident, count });
        std.debug.print("added {s}\n", .{gop.value_ptr.*});
    }
    return @intCast(gop.index);
}

fn visitTemplateElaboratedIdent(self: *Self, tree: *Ast, node: Node.Index) !void {
    if (node == 0) return;
    tree.nodes.items(.rhs)[node] = try self.getOrAddGlobalAlias(tree.nodeSource(node));
    const lhs = tree.nodeLHS(node);
    if (lhs != 0) {
        for (tree.spanToList(lhs)) |n| try self.visitExpr(tree, n);
    }
}

fn visitType(self: *Self, tree: *Ast, node: Node.Index) Allocator.Error!void {
    try self.visitTemplateElaboratedIdent(tree, node);
}

fn visitCall(self: *Self, tree: *Ast, node: Node.Index) Allocator.Error!void {
    try self.visitType(tree, tree.nodeLHS(node));

    const args = tree.nodeRHS(node);
    if (args != 0) {
        for (tree.spanToList(args)) |arg| try self.visitExpr(tree, arg);
    }
}

fn visitExpr(self: *Self, tree: *Ast, node: Node.Index) Allocator.Error!void {
    if (node == 0) return;
    const lhs = tree.nodeLHS(node);
    const rhs = tree.nodeRHS(node);
    switch (tree.nodeTag(node)) {
        .ident => {
            tree.nodes.items(.rhs)[node] = try self.getOrAddGlobalAlias(tree.nodeSource(node));
        },
        .number, .true, .false => {},
        .not, .negate, .deref, .addr_of, .paren_expr, .field_access => try self.visitExpr(tree, lhs),
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
            try self.visitExpr(tree, lhs);
            try self.visitExpr(tree, rhs);
        },
        .call => try self.visitCall(tree, node),
        else => |t| {
            std.debug.print("invalid expression {s}\n", .{@tagName(t)});
        },
    }
}

fn visitVar(self: *Self, tree: *Ast, extra: anytype, node: Node.Index) Allocator.Error!void {
    if (extra.type != 0) try self.visitType(tree, extra.type);
    try self.visitExpr(tree, tree.nodeRHS(node));
}

fn visitGlobalVar(self: *Self, tree: *Ast, node: Node.Index) Allocator.Error!void {
    const extra = tree.extraData(Node.GlobalVar, tree.nodeLHS(node));
    try self.visitVar(tree, extra, node);
}

fn visitGlobalDecl(self: *Self, tree: *Ast, node: Node.Index) Allocator.Error!void {
    switch (tree.nodeTag(node)) {
        .global_var => try self.visitGlobalVar(tree, node),
        // .override => try self.visitOverride(tree, node),
        // .@"const" => try self.visitConst(tree, node),
        // .const_assert => try self.visitConstAssert(tree, node),
        // .type_alias => try self.visitTypeAlias(tree, node),
        // .@"struct" => try self.visitStruct(tree, node),
        // .@"fn" => try self.visitFn(tree, node),
        // .comment => {},
        else => |t| {
            std.debug.print("could not alias node {s}\n", .{@tagName(t)});
        },
    }
}

fn visitDirective(self: *Self, tree: *Ast, node: Node.Index) Allocator.Error!void {
    switch (tree.nodeTag(node)) {
        .enable_directive, .requires_directive => {
            for (tree.spanToList(tree.nodeLHS(node))) |ext| {
                try self.directives.enables.put(tree.tokenSource(ext), {});
            }
        },
        .diagnostic_directive => {},
        else => {},
    }
}

/// Add module to intermediate data structures.
pub fn alias(self: *Self, tree: *Ast) !void {
    for (tree.spanToList(0)) |node| {
        switch (tree.nodeTag(node)) {
            .enable_directive, .requires_directive, .diagnostic_directive => try self.visitDirective(tree, node),
            .global_var, .override, .@"const", .@"struct", .@"fn", .type_alias => try self.visitGlobalDecl(tree, node),
            else => {},
        }
    }
}

pub fn finish(self: *Self) !Ast {
    self.deinit();

    return Ast{ .source = try self.source.toOwnedSliceSentinel(0), .tokens = self.tokens.toOwnedSlice(), .nodes = self.nodes.toOwnedSlice(), .extra = try self.extra.toOwnedSlice(self.allocator), .errors = &.{} };
}
