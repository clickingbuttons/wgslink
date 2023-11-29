const std = @import("std");
const Ast = @import("./Ast.zig");
const AstBuilder = @import("./Builder.zig");
const node_mod = @import("./Node.zig");

const Self = @This();
const Allocator = std.mem.Allocator;
const Node = node_mod.Node;
// { [scope key]: [global alias] }
const Scope = std.StringArrayHashMap(Node.IdentIndex);
const Scopes = std.ArrayList(Scope);
const Directives = struct {
    const StringSet = std.StringArrayHashMap(void);
    const Diagnostics = std.AutoArrayHashMap(node_mod.DiagnosticControl, void);
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
builder: AstBuilder = .{},
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
    };
    // Reserve 0th index for root
    _ = try res.addNode(Node{ .span = .{ .from = 0, .to = 0 } });
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
    return try self.builder.listToSpan(self.allocator, list);
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
            const alias = self.builder.identifiers.keys()[gop.value_ptr.*];
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
    var typed_ident = tree.extraData(node_mod.TypedIdent, index);
    typed_ident.name = try self.getOrPutIdent(ty, tree.identifier(typed_ident.name));
    typed_ident.type = try self.visit(tree, typed_ident.type);
    return try self.addExtra(typed_ident);
}

pub fn toOwnedAst(self: *Self, lang: Ast.Language) Error!Ast {
    std.debug.assert(self.scratch.items.len == 0);

    const enables = self.directives.enables.keys();
    if (enables.len > 0) {
        const idents = try self.identList(enables);
        const node = try self.addNode(Node{ .enable_directive = .{ .idents = idents } });
        try self.scratch.append(self.allocator, node);
    }
    const requires = self.directives.requires.keys();
    if (requires.len > 0) {
        const idents = try self.identList(requires);
        const node = try self.addNode(Node{ .requires_directive = .{ .idents = idents } });
        try self.scratch.append(self.allocator, node);
    }
    for (self.directives.diagnostics.keys()) |d| {
        const control = try self.addExtra(d);
        const node = try self.addNode(Node{ .diagnostic_directive = .{ .diagnostic_control = control } });
        try self.scratch.append(self.allocator, node);
    }

    var extra = &self.builder.extra;
    try extra.appendSlice(self.allocator, self.scratch.items);
    try extra.appendSlice(self.allocator, self.roots.items);

    const total_len = self.scratch.items.len + self.roots.items.len;
    var span = &self.builder.nodes.items(.data)[0].span;
    span.from = @intCast(extra.items.len - total_len);
    span.to = @intCast(extra.items.len);

    return self.builder.toOwnedAst(self.allocator, lang);
}

fn diagnosticControl(
    self: *Self,
    tree: Ast,
    index: Node.ExtraIndex,
) Error!node_mod.DiagnosticControl {
    var diagnostic = tree.extraData(node_mod.DiagnosticControl, index);
    const name = tree.identifier(diagnostic.name);
    diagnostic.name = try self.getOrPutIdent(.token, name);
    if (diagnostic.field != 0) {
        const field = tree.identifier(diagnostic.field);
        diagnostic.field = try self.getOrPutIdent(.token, field);
    }
    return diagnostic;
}

pub fn appendComment(self: *Self, comment: []const u8) Error!void {
    const node = try self.addNode(Node{ .comment = .{
        .ident = try self.getOrPutIdent(.token, comment),
    } });
    try self.roots.append(self.allocator, node);
}

fn visit(self: *Self, tree: Ast, index: Node.Index) Error!Node.Index {
    if (index == 0) return 0;
    const tree_node = tree.node(index);
    const node: Node = switch (tree_node) {
        .@"error", .import, .import_alias, .comment => unreachable,
        .span => {
            const scratch_top = self.scratch.items.len;
            defer self.scratch.shrinkRetainingCapacity(scratch_top);
            for (tree.spanToList(index)) |i| {
                try self.scratch.append(self.allocator, try self.visit(tree, i));
            }

            return try self.listToSpan(self.scratch.items[scratch_top..]);
        },
        .enable_directive, .requires_directive => |n| {
            for (tree.spanToList(n.idents)) |i| {
                const name = tree.identifier(i);
                switch (tree_node) {
                    .enable_directive => try self.directives.enables.put(name, {}),
                    .requires_directive => try self.directives.requires.put(name, {}),
                    else => unreachable,
                }
            }
            return 0;
        },
        .diagnostic_directive => |n| {
            const diagnostic = try self.diagnosticControl(tree, n.diagnostic_control);
            try self.directives.diagnostics.put(diagnostic, {});
            return 0;
        },
        .global_var => |n| brk: {
            var global_var = tree.extraData(node_mod.GlobalVar, n.global_var);
            const initializer = try self.visit(tree, n.initializer);
            global_var.name = try self.getOrPutIdent(.decl, tree.identifier(global_var.name));
            global_var.attrs = try self.visit(tree, global_var.attrs);
            global_var.template_list = try self.visit(tree, global_var.template_list);
            global_var.type = try self.visit(tree, global_var.type);
            break :brk Node{ .global_var = .{
                .global_var = try self.addExtra(global_var),
                .initializer = initializer,
            } };
        },
        .override => |n| brk: {
            var override = tree.extraData(node_mod.Override, n.override);
            const initializer = try self.visit(tree, n.initializer);
            override.name = try self.getOrPutIdent(.decl, tree.identifier(override.name));
            override.attrs = try self.visit(tree, override.attrs);
            override.type = try self.visit(tree, override.type);
            break :brk Node{ .override = .{
                .override = try self.addExtra(override),
                .initializer = initializer,
            } };
        },
        .@"fn" => |n| brk: {
            var header = tree.extraData(node_mod.FnHeader, n.fn_header);
            header.name = try self.getOrPutIdent(.decl, tree.identifier(header.name));
            header.attrs = try self.visit(tree, header.attrs);
            header.params = try self.visit(tree, header.params);
            header.return_attrs = try self.visit(tree, header.return_attrs);
            header.return_type = try self.visit(tree, header.return_type);
            break :brk Node{ .@"fn" = .{
                .fn_header = try self.addExtra(header),
                .body = try self.visit(tree, n.body),
            } };
        },
        .@"const", .let => |n| brk: {
            const initializer = try self.visit(tree, n.initializer);
            const let = Node.Let{
                .typed_ident = try self.typedIdent(.decl, tree, n.typed_ident),
                .initializer = initializer,
            };
            break :brk switch (tree_node) {
                .@"const" => Node{ .@"const" = let },
                .let => Node{ .let = let },
                else => unreachable,
            };
        },
        .type_alias => |n| Node{ .type_alias = .{
            .new_name = try self.getOrPutIdent(.decl, tree.identifier(n.new_name)),
            .old_type = try self.visit(tree, n.old_type),
        } },
        .@"struct" => |n| Node{ .@"struct" = .{
            .name = try self.getOrPutIdent(.decl, tree.identifier(n.name)),
            .members = try self.visit(tree, n.members),
        } },
        .struct_member => |n| Node{ .struct_member = .{
            .attributes = try self.visit(tree, n.attributes),
            .typed_ident = try self.typedIdent(.token, tree, n.typed_ident),
        } },
        .fn_param => |n| brk: {
            var param = tree.extraData(node_mod.FnParam, n.fn_param);
            param.name = try self.getOrPutIdent(.decl, tree.identifier(param.name));
            param.type = try self.visit(tree, param.type);
            break :brk Node{ .fn_param = .{
                .fn_param = try self.addExtra(param),
                .attributes = try self.visit(tree, n.attributes),
            } };
        },
        .type, .ident => |n| brk: {
            const ident = Node.Ident{
                .name = try self.getOrPutIdent(.ref, tree.identifier(n.name)),
                .template_list = try self.visit(tree, n.template_list),
            };
            break :brk switch (tree_node) {
                .type => Node{ .type = ident },
                .ident => Node{ .ident = ident },
                else => unreachable,
            };
        },
        .attribute => |n| brk: {
            const attr = switch (n) {
                inline .@"const",
                .compute,
                .fragment,
                .invariant,
                .must_use,
                .vertex,
                => |_, tag| @unionInit(node_mod.Attribute, @tagName(tag), {}),
                .diagnostic => |d| brk2: {
                    const diagnostic = try self.diagnosticControl(tree, d);
                    const extra = try self.addExtra(diagnostic);
                    break :brk2 node_mod.Attribute{ .diagnostic = extra };
                },
                .interpolate => |i| brk2: {
                    var interpolate = tree.extraData(node_mod.Interpolation, i);
                    interpolate.type = try self.visit(tree, interpolate.type);
                    interpolate.sampling_expr = try self.visit(tree, interpolate.sampling_expr);
                    const extra = try self.addExtra(interpolate);
                    break :brk2 node_mod.Attribute{ .interpolate = extra };
                },
                .@"align" => |e| node_mod.Attribute{ .@"align" = try self.visit(tree, e) },
                .binding => |e| node_mod.Attribute{ .binding = try self.visit(tree, e) },
                .builtin => |e| node_mod.Attribute{ .builtin = try self.visit(tree, e) },
                .group => |e| node_mod.Attribute{ .group = try self.visit(tree, e) },
                .id => |e| node_mod.Attribute{ .id = try self.visit(tree, e) },
                .location => |e| node_mod.Attribute{ .location = try self.visit(tree, e) },
                .size => |e| node_mod.Attribute{ .size = try self.visit(tree, e) },
                .workgroup_size => |w| brk2: {
                    var workgroup_size = tree.extraData(node_mod.WorkgroupSize, w);
                    workgroup_size.x = try self.visit(tree, workgroup_size.x);
                    workgroup_size.y = try self.visit(tree, workgroup_size.y);
                    workgroup_size.z = try self.visit(tree, workgroup_size.z);
                    const extra = try self.addExtra(workgroup_size);
                    break :brk2 node_mod.Attribute{ .workgroup_size = extra };
                },
            };
            break :brk Node{ .attribute = attr };
        },
        .@"for" => |n| brk: {
            var header = tree.extraData(node_mod.ForHeader, n.for_header);
            header.attrs = try self.visit(tree, header.attrs);
            header.cond = try self.visit(tree, header.cond);
            header.init = try self.visit(tree, header.init);
            header.update = try self.visit(tree, header.update);
            break :brk Node{ .@"for" = .{
                .for_header = try self.addExtra(header),
                .body = try self.visit(tree, n.body),
            } };
        },
        .loop => |n| Node{ .loop = .{
            .attributes = try self.visit(tree, n.attributes),
            .body = try self.visit(tree, n.body),
        } },
        .compound => |n| brk: {
            const attributes = try self.visit(tree, n.attributes);
            try self.pushScope();
            const statements = try self.visit(tree, n.statements);
            self.popScope();
            break :brk Node{ .compound = .{ .attributes = attributes, .statements = statements } };
        },
        .@"if" => |n| Node{ .@"if" = .{
            .condition = try self.visit(tree, n.condition),
            .body = try self.visit(tree, n.body),
        } },
        .else_if => |n| Node{ .else_if = .{
            .if1 = try self.visit(tree, n.if1),
            .if2 = try self.visit(tree, n.if2),
        } },
        .@"else" => |n| Node{ .@"else" = .{
            .@"if" = try self.visit(tree, n.@"if"),
            .body = try self.visit(tree, n.body),
        } },
        .@"switch" => |n| Node{ .@"switch" = .{
            .expr = try self.visit(tree, n.expr),
            .switch_body = try self.visit(tree, n.switch_body),
        } },
        .switch_body => |n| Node{ .switch_body = .{
            .attributes = try self.visit(tree, n.attributes),
            .clauses = try self.visit(tree, n.clauses),
        } },
        .case_clause => |n| Node{ .case_clause = .{
            .selectors = try self.visit(tree, n.selectors),
            .body = try self.visit(tree, n.body),
        } },
        .case_selector => |n| Node{ .case_selector = .{ .expr = try self.visit(tree, n.expr) } },
        .@"while" => |n| Node{ .@"while" = .{
            .condition = try self.visit(tree, n.condition),
            .body = try self.visit(tree, n.body),
        } },
        .@"return" => |n| Node{ .@"return" = .{ .expr = try self.visit(tree, n.expr) } },
        .continuing => |n| Node{ .continuing = .{ .body = try self.visit(tree, n.body) } },
        .break_if => |n| Node{ .break_if = .{ .condition = try self.visit(tree, n.condition) } },
        .call => |n| Node{ .call = .{
            .ident = try self.visit(tree, n.ident),
            .arguments = try self.visit(tree, n.arguments),
        } },
        .@"var" => |n| brk: {
            var extra = tree.extraData(node_mod.Var, n.@"var");
            const initializer = try self.visit(tree, n.initializer);
            extra.name = try self.getOrPutIdent(.decl, tree.identifier(extra.name));
            extra.template_list = try self.visit(tree, extra.template_list);
            extra.type = try self.visit(tree, extra.type);
            break :brk Node{ .@"var" = .{
                .@"var" = try self.addExtra(extra),
                .initializer = initializer,
            } };
        },
        // zig fmt off
        inline .const_assert,
        .increment,
        .decrement,
        .phony_assign,
        .paren,
        .logical_not,
        .bitwise_complement,
        .negative,
        .deref,
        .ref,
        // zig fmt on
        => |n, tag| @unionInit(Node, @tagName(tag), Node.SingleExpr{
            .expr = try self.visit(tree, n.expr),
        }),
        inline .@"=",
        .@"+=",
        .@"-=",
        .@"*=",
        .@"/=",
        .@"%=",
        .@"&=",
        .@"|=",
        .@"^=",
        .@"<<=",
        .@">>=",
        => |n, tag| @unionInit(Node, @tagName(tag), Node.Assign{
            .lhs_expr = try self.visit(tree, n.lhs_expr),
            .rhs_expr = try self.visit(tree, n.rhs_expr),
        }),
        inline .lshift, .rshift => |n, tag| brk: {
            break :brk @unionInit(Node, @tagName(tag), Node.ShiftExpr{
                .lhs_unary_expr = try self.visit(tree, n.lhs_unary_expr),
                .rhs_unary_expr = try self.visit(tree, n.rhs_unary_expr),
            });
        },
        inline .lt, .gt, .lte, .gte, .eq, .neq => |n, tag| brk: {
            break :brk @unionInit(Node, @tagName(tag), Node.RelationalExpr{
                .lhs_shift_expr = try self.visit(tree, n.lhs_shift_expr),
                .rhs_shift_expr = try self.visit(tree, n.rhs_shift_expr),
            });
        },
        inline .mul, .div, .mod => |n, tag| brk: {
            break :brk @unionInit(Node, @tagName(tag), Node.MultiplicativeExpr{
                .lhs_multiplicative_expr = try self.visit(tree, n.lhs_multiplicative_expr),
                .rhs_unary_expr = try self.visit(tree, n.rhs_unary_expr),
            });
        },
        inline .add, .sub => |n, tag| brk: {
            break :brk @unionInit(Node, @tagName(tag), Node.AdditiveExpr{
                .lhs_additive_expr = try self.visit(tree, n.lhs_additive_expr),
                .rhs_mul_expr = try self.visit(tree, n.rhs_mul_expr),
            });
        },
        inline .logical_and, .logical_or => |n, tag| brk: {
            break :brk @unionInit(Node, @tagName(tag), Node.ShortCircuitExpr{
                .lhs_relational_expr = try self.visit(tree, n.lhs_relational_expr),
                .rhs_relational_expr = try self.visit(tree, n.rhs_relational_expr),
            });
        },
        inline .bitwise_and, .bitwise_or, .bitwise_xor => |n, tag| brk: {
            break :brk @unionInit(Node, @tagName(tag), Node.BitwiseExpr{
                .lhs_bitwise_expr = try self.visit(tree, n.lhs_bitwise_expr),
                .rhs_unary_expr = try self.visit(tree, n.rhs_unary_expr),
            });
        },
        .field_access => |n| Node{ .field_access = .{
            .lhs_expr = try self.visit(tree, n.lhs_expr),
            .member = try self.getOrPutIdent(.token, tree.identifier(n.member)),
        } },
        .index_access => |n| Node{ .index_access = .{
            .lhs_expr = try self.visit(tree, n.lhs_expr),
            .index_expr = try self.visit(tree, n.index_expr),
        } },
        .number => |n| Node{ .number = .{
            .value = try self.getOrPutIdent(.token, tree.identifier(n.value)),
        } },
        inline .true, .false, .@"break", .@"continue", .discard => |_, tag| brk: {
            break :brk @unionInit(Node, @tagName(tag), {});
        },
    };

    return try self.addNode(node);
}
