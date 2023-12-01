const std = @import("std");
const Ast = @import("./Ast.zig");
const AstBuilder = @import("./Builder.zig");
const Node = @import("./Node.zig");
const File = @import("../file/File.zig");

const Self = @This();
const Allocator = std.mem.Allocator;
// { [scope key]: [global alias] }
const Scope = std.StringArrayHashMap(Node.IdentIndex);
const Scopes = std.ArrayList(Scope);
const Directives = struct {
    const StringSet = std.StringArrayHashMap(void);
    const Diagnostics = std.AutoArrayHashMap(Node.DiagnosticControl, void);
    enables: StringSet,
    requires: StringSet,
    diagnostics: Diagnostics,

    pub fn init(allocator: Allocator) Directives {
        return Directives{
            .enables = StringSet.init(allocator),
            .requires = StringSet.init(allocator),
            .diagnostics = Diagnostics.init(allocator),
        };
    }

    pub fn deinit(self: *Directives) void {
        self.enables.deinit();
        self.requires.deinit();
        self.diagnostics.deinit();
    }
};
const AliasingError = enum {
    symbol_already_declared,
};
const Error = Allocator.Error || error{Aliasing};

allocator: Allocator,
scopes: Scopes,
minify: bool,

/// Accumulator since MUST be at top of WGSL file
directives: Directives,
/// The main event
builder: AstBuilder,
/// Used to build lists
scratch: std.ArrayListUnmanaged(Node.Index) = .{},
roots: std.ArrayListUnmanaged(Node.Index) = .{},

pub fn init(allocator: Allocator, minify: bool) !Self {
    var scopes = Scopes.init(allocator);
    // Global scope
    try scopes.append(Scope.init(allocator));
    var res = Self{
        .allocator = allocator,
        .scopes = scopes,
        .minify = minify,
        .directives = Directives.init(allocator),
        .builder = try AstBuilder.init(allocator),
    };
    return res;
}

pub fn deinit(self: *Self) void {
    for (self.scopes.items) |*s| s.deinit();
    self.scopes.deinit();
    self.directives.deinit();
    self.builder.deinit(self.allocator);
    self.scratch.deinit(self.allocator);
    self.roots.deinit(self.allocator);
}

/// Add module to intermediate data structures.
pub fn append(self: *Self, tree: Ast) !void {
    for (tree.spanToList(0)) |n| {
        const index = try self.visit(tree, n);
        if (index != 0) try self.roots.append(self.allocator, index);
    }
}

fn listToSpan(self: *Self, list: []const Node.Index) Error!Node.Index {
    return try self.builder.listToSpan(self.allocator, 0, list);
}

fn addNode(self: *Self, node: Node) Error!Node.Index {
    return try self.builder.addNode(self.allocator, node);
}

fn addExtra(self: *Self, extra: anytype) Error!Node.Index {
    return try self.builder.addExtra(self.allocator, extra);
}

fn pushScope(self: *Self) Allocator.Error!void {
    try self.scopes.append(Scope.init(self.allocator));
}

fn popScope(self: *Self) void {
    var s = self.scopes.pop();
    s.deinit();
}

const IdentType = enum {
    /// var foo
    decl,
    /// foo + 4
    ref,
    /// requires foo
    token,
};

fn isUniqueIdent(self: *Self, ident: []const u8) bool {
    for (self.scopes.items) |s| {
        if (s.get(ident) != null) return false;
    }

    return true;
}

/// Caller owns returned slice
fn makeUniqueIdent(self: *Self, ident: []const u8) ![]const u8 {
    var count: usize = 2;
    var new_ident = try std.fmt.allocPrint(self.allocator, "{s}{d}", .{ ident, count });
    while (!self.isUniqueIdent(new_ident)) {
        count += 1;
        self.allocator.free(new_ident);
        new_ident = try std.fmt.allocPrint(self.allocator, "{s}{d}", .{ ident, count });
    }

    return new_ident;
}

fn getOrPutIdent(
    self: *Self,
    comptime ty: IdentType,
    ident: []const u8,
) Error!Node.IdentIndex {
    switch (ty) {
        .decl => {
            const scope_index = self.scopes.items.len - 1;
            var scope: *Scope = &self.scopes.items[scope_index];
            var gop = try scope.getOrPut(ident);

            if (gop.found_existing) {
                std.debug.print("decl {s} found_existing\n", .{ident});
                for (scope.keys()) |k| std.debug.print("in scope {s}\n", .{k});
                const alias = try self.makeUniqueIdent(ident);
                defer self.allocator.free(alias);
                gop.value_ptr.* = try self.builder.getOrPutIdent(self.allocator, alias);
            } else {
                gop.value_ptr.* = try self.builder.getOrPutIdent(self.allocator, ident);
            }
            const alias = self.builder.identifiers.keys()[gop.value_ptr.* - 1];
            try scope.put(alias, gop.value_ptr.*);

            std.debug.print("decl {s} alias {s} index {d}\n", .{ ident, alias, gop.value_ptr.* });
            return gop.value_ptr.*;
        },
        .ref => {
            for (0..self.scopes.items.len) |i| {
                const s = self.scopes.items[self.scopes.items.len - i - 1];
                if (s.get(ident)) |index| {
                    std.debug.print("ref {s} index {d}\n", .{ ident, index });
                    return index;
                }
            }
            std.debug.print("ref {s}\n", .{ident});
        },
        .token => {},
    }
    return try self.builder.getOrPutIdent(self.allocator, ident);
}

inline fn identList(self: *Self, idents: [][]const u8) Error!Node.Index {
    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);
    for (idents) |e| {
        const ident = try self.getOrPutIdent(.token, e);
        try self.scratch.append(self.allocator, ident);
    }
    return try self.listToSpan(self.scratch.items[scratch_top..]);
}

inline fn typedIdent(
    self: *Self,
    comptime ty: IdentType,
    tree: Ast,
    index: Node.Index,
) Error!Node.Index {
    var typed_ident = tree.extraData(Node.TypedIdent, index);
    typed_ident.name = try self.getOrPutIdent(ty, tree.identifier(typed_ident.name));
    typed_ident.type = try self.visit(tree, typed_ident.type);
    return try self.addExtra(typed_ident);
}

pub fn toOwnedAst(self: *Self, lang: File.Language) Error!Ast {
    std.debug.assert(self.scratch.items.len == 0);

    const enables = self.directives.enables.keys();
    if (enables.len > 0) {
        const idents = try self.identList(enables);
        const node = try self.addNode(Node{ .tag = .enable_directive, .lhs = idents });
        try self.scratch.append(self.allocator, node);
    }
    const requires = self.directives.requires.keys();
    if (requires.len > 0) {
        const idents = try self.identList(requires);
        const node = try self.addNode(Node{ .tag = .requires_directive, .lhs = idents });
        try self.scratch.append(self.allocator, node);
    }
    for (self.directives.diagnostics.keys()) |d| {
        const control = try self.addExtra(d);
        const node = try self.addNode(Node{ .tag = .diagnostic_directive, .lhs = control });
        try self.scratch.append(self.allocator, node);
    }

    var extra = &self.builder.extra;
    try extra.appendSlice(self.allocator, self.scratch.items);
    try extra.appendSlice(self.allocator, self.roots.items);
    self.builder.finishRootSpan(self.scratch.items.len + self.roots.items.len);

    return self.builder.toOwnedAst(self.allocator, lang);
}

fn diagnosticControl(
    self: *Self,
    tree: Ast,
    index: Node.ExtraIndex,
) Error!Node.DiagnosticControl {
    var diagnostic = tree.extraData(Node.DiagnosticControl, index);
    const name = tree.identifier(diagnostic.name);
    diagnostic.name = try self.getOrPutIdent(.token, name);
    if (diagnostic.field != 0) {
        const field = tree.identifier(diagnostic.field);
        diagnostic.field = try self.getOrPutIdent(.token, field);
    }
    return diagnostic;
}

pub fn appendComment(self: *Self, comment: []const u8) Error!void {
    const text = try self.getOrPutIdent(.token, comment);
    const node = try self.addNode(Node{ .tag = .comment, .lhs = text });
    try self.roots.append(self.allocator, node);
}

fn visit(self: *Self, tree: Ast, index: Node.Index) Error!Node.Index {
    if (index == 0) return 0;
    var node = tree.node(index);

    switch (node.tag) {
        .@"error", .comment => unreachable,
        .span => {
            const scratch_top = self.scratch.items.len;
            defer self.scratch.shrinkRetainingCapacity(scratch_top);
            for (tree.spanToList(index)) |i| {
                try self.scratch.append(self.allocator, try self.visit(tree, i));
            }

            return try self.listToSpan(self.scratch.items[scratch_top..]);
        },
        .enable_directive, .requires_directive => |t| {
            for (tree.spanToList(node.lhs)) |i| {
                const name = tree.identifier(i);
                switch (t) {
                    .enable_directive => try self.directives.enables.put(name, {}),
                    .requires_directive => try self.directives.requires.put(name, {}),
                    else => unreachable,
                }
            }
            return 0;
        },
        .diagnostic_directive => {
            const diagnostic = try self.diagnosticControl(tree, node.lhs);
            try self.directives.diagnostics.put(diagnostic, {});
            return 0;
        },
        .global_var => {
            var global_var = tree.extraData(Node.GlobalVar, node.lhs);
            global_var.name = try self.getOrPutIdent(.decl, tree.identifier(global_var.name));
            global_var.attrs = try self.visit(tree, global_var.attrs);
            global_var.template_list = try self.visit(tree, global_var.template_list);
            global_var.type = try self.visit(tree, global_var.type);
            node.lhs = try self.addExtra(global_var);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .override => {
            var override = tree.extraData(Node.Override, node.lhs);
            override.name = try self.getOrPutIdent(.decl, tree.identifier(override.name));
            override.attrs = try self.visit(tree, override.attrs);
            override.type = try self.visit(tree, override.type);
            node.lhs = try self.addExtra(override);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .@"fn" => {
            var header = tree.extraData(Node.FnHeader, node.lhs);
            header.name = try self.getOrPutIdent(.decl, tree.identifier(header.name));
            header.attrs = try self.visit(tree, header.attrs);
            header.params = try self.visit(tree, header.params);
            header.return_attrs = try self.visit(tree, header.return_attrs);
            header.return_type = try self.visit(tree, header.return_type);
            node.lhs = try self.addExtra(header);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .fn_param => {
            var param = tree.extraData(Node.FnParam, node.rhs);
            param.name = try self.getOrPutIdent(.decl, tree.identifier(param.name));
            param.type = try self.visit(tree, param.type);
            node.lhs = try self.visit(tree, node.lhs);
            node.rhs = try self.addExtra(param);
        },
        .@"var" => {
            var extra = tree.extraData(Node.Var, node.lhs);
            extra.name = try self.getOrPutIdent(.decl, tree.identifier(extra.name));
            extra.template_list = try self.visit(tree, extra.template_list);
            extra.type = try self.visit(tree, extra.type);
            node.lhs = try self.addExtra(extra);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .@"for" => {
            var header = tree.extraData(Node.ForHeader, node.lhs);
            header.attrs = try self.visit(tree, header.attrs);
            header.cond = try self.visit(tree, header.cond);
            header.init = try self.visit(tree, header.init);
            header.update = try self.visit(tree, header.update);
            node.lhs = try self.addExtra(header);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .import_alias => {
            node.lhs = try self.getOrPutIdent(.token, tree.identifier(node.lhs));
            if (node.rhs != 0) {
                node.rhs = try self.getOrPutIdent(.decl, tree.identifier(node.rhs));
            }
        },
        .type_alias => {
            node.lhs = try self.getOrPutIdent(.decl, tree.identifier(node.lhs));
            node.rhs = try self.visit(tree, node.rhs);
        },
        .type, .ident => {
            node.lhs = try self.getOrPutIdent(.ref, tree.identifier(node.lhs));
            node.rhs = try self.visit(tree, node.rhs);
        },
        .@"struct" => {
            node.lhs = try self.getOrPutIdent(.decl, tree.identifier(node.lhs));
            node.rhs = try self.visit(tree, node.rhs);
        },
        .import => {
            node.lhs = try self.visit(tree, node.lhs);
            node.rhs = try self.getOrPutIdent(.token, tree.identifier(node.rhs));
        },
        .field_access => {
            node.lhs = try self.visit(tree, node.lhs);
            node.rhs = try self.getOrPutIdent(.token, tree.identifier(node.rhs));
        },
        .number => {
            node.lhs = try self.getOrPutIdent(.token, tree.identifier(node.lhs));
        },
        .@"const", .let => {
            node.lhs = try self.typedIdent(.decl, tree, node.lhs);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .attribute => {
            const attr: Node.Attribute = @enumFromInt(node.lhs);
            switch (attr) {
                .@"const", .compute, .fragment, .invariant, .must_use, .vertex => {},
                .diagnostic => {
                    const diagnostic = try self.diagnosticControl(tree, node.rhs);
                    node.rhs = try self.addExtra(diagnostic);
                },
                .interpolate => {
                    var interpolate = tree.extraData(Node.Interpolation, node.rhs);
                    interpolate.type = try self.visit(tree, interpolate.type);
                    interpolate.sampling_expr = try self.visit(tree, interpolate.sampling_expr);
                    node.rhs = try self.addExtra(interpolate);
                },
                .@"align",
                .binding,
                .builtin,
                .group,
                .id,
                .location,
                .size,
                => {
                    node.rhs = try self.visit(tree, node.rhs);
                },
                .workgroup_size => {
                    var workgroup_size = tree.extraData(Node.WorkgroupSize, node.rhs);
                    workgroup_size.x = try self.visit(tree, workgroup_size.x);
                    workgroup_size.y = try self.visit(tree, workgroup_size.y);
                    workgroup_size.z = try self.visit(tree, workgroup_size.z);
                    node.rhs = try self.addExtra(workgroup_size);
                },
            }
        },
        .compound => {
            node.lhs = try self.visit(tree, node.lhs);
            try self.pushScope();
            node.rhs = try self.visit(tree, node.rhs);
            self.popScope();
        },
        .struct_member => {
            node.lhs = try self.visit(tree, node.lhs);
            node.rhs = try self.typedIdent(.token, tree, node.rhs);
        },
        .index_access, .const_assert, .increment, .decrement, .phony_assign, .paren, .logical_not, .bitwise_complement, .negative, .deref, .@"return", .break_if, .continuing, .ref, .loop, .@"if", .else_if, .@"else", .@"switch", .switch_body, .case_clause, .case_selector, .@"while", .call, .@"=", .@"+=", .@"-=", .@"*=", .@"/=", .@"%=", .@"&=", .@"|=", .@"^=", .@"<<=", .@">>=", .lshift, .rshift, .lt, .gt, .lte, .gte, .eq, .neq, .mul, .div, .mod, .add, .sub, .logical_and, .logical_or, .bitwise_and, .bitwise_or, .bitwise_xor => {
            node.lhs = try self.visit(tree, node.lhs);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .true, .false, .@"break", .@"continue", .discard => {},
    }

    return try self.addNode(node);
}
