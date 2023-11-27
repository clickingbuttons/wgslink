const std = @import("std");
const Ast = @import("./Ast.zig");
const node_mod = @import("./Node.zig");

const Node = node_mod.Node;
const Allocator = std.mem.Allocator;
const Used = std.StringHashMap(void);

fn findSymbols(used: *Used, tree: *Ast) !void {
    for (tree.spanToList(0)) |i| {
        switch (tree.node(i)) {
            .@"fn" => |n| {
                const header = tree.extraData(node_mod.FnHeader, n.fn_header);
                if (header.attrs == 0) continue;
                for (tree.spanToList(header.attrs)) |a| {
                    const attr = tree.node(a).attribute;
                    switch (attr) {
                        .vertex, .fragment, .compute => {
                            try used.put(tree.globalName(i), {});
                            break;
                        },
                        else => {},
                    }
                }
            },
            else => {},
        }
    }
}

fn addUsedSymbol(tree: *Ast, used: *Used, ident: Node.IdentIndex) Allocator.Error!void {
    const symbol = tree.identifier(ident);
    if (symbol.len == 0 or used.get(symbol) != null) return;
    try used.put(symbol, {});
    for (tree.spanToList(0)) |i| {
        switch (tree.node(i)) {
            .global_var, .override, .@"const", .@"struct", .@"fn", .type_alias => {
                if (std.mem.eql(u8, tree.globalName(i), symbol)) try visit(tree, used, i);
            },
            else => {},
        }
    }
}

fn visitAll(tree: *Ast, used: *Used, nodes: []const Node.Index) Allocator.Error!void {
    for (nodes) |n| try visit(tree, used, n);
}

fn visit(tree: *Ast, used: *Used, node: Node.Index) Allocator.Error!void {
    if (node == 0) return;
    switch (tree.node(node)) {
        .span => for (tree.spanToList(node)) |i| try visit(tree, used, i),
        .global_var => |n| {
            const global_var = tree.extraData(node_mod.GlobalVar, n.global_var);
            try visitAll(tree, used, &.{
                global_var.attrs,
                global_var.template_list,
                global_var.type,
                n.initializer,
            });
        },
        .override => |n| {
            const override = tree.extraData(node_mod.Override, n.override);
            try visitAll(tree, used, &.{
                override.attrs,
                override.type,
                n.initializer,
            });
        },
        .@"fn" => |n| {
            const header = tree.extraData(node_mod.FnHeader, n.fn_header);
            try visitAll(tree, used, &.{
                header.attrs,
                header.params,
                header.return_attrs,
                header.return_type,
                n.body,
            });
        },
        .@"const", .let => |n| {
            const typed_ident = tree.extraData(node_mod.TypedIdent, n.typed_ident);
            try visitAll(tree, used, &.{
                typed_ident.name,
                typed_ident.type,
                n.initializer,
            });
        },
        .type_alias => |n| try visit(tree, used, n.old_type),
        .@"struct" => |n| try visit(tree, used, n.members),
        .struct_member => |n| {
            const typed_ident = tree.extraData(node_mod.TypedIdent, n.typed_ident);
            try visitAll(tree, used, &.{
                n.attributes,
                typed_ident.type,
            });
        },
        .fn_param => |n| {
            const param = tree.extraData(node_mod.FnParam, n.fn_param);
            try visitAll(tree, used, &.{
                n.attributes,
                param.type,
            });
        },
        .type, .ident => |n| {
            try addUsedSymbol(tree, used, n.name);
            try visit(tree, used, n.template_list);
        },
        .attribute => |n| {
            switch (n) {
                .compute, .@"const", .fragment, .invariant, .must_use, .vertex, .diagnostic, .interpolate => {},
                .@"align", .binding, .builtin, .group, .id, .location, .size => |e| {
                    try visit(tree, used, e);
                },
                .workgroup_size => |w| {
                    const workgroup_size = tree.extraData(node_mod.WorkgroupSize, w);
                    try visitAll(tree, used, &.{
                        workgroup_size.x,
                        workgroup_size.y,
                        workgroup_size.z,
                    });
                },
            }
        },
        .@"for" => |n| {
            const header = tree.extraData(node_mod.ForHeader, n.for_header);
            try visitAll(tree, used, &.{
                header.attrs,
                header.init,
                header.cond,
                header.update,
                n.body,
            });
        },
        .loop => |n| try visitAll(tree, used, &.{ n.attributes, n.body }),
        .compound => |n| try visit(tree, used, n.statements),
        .@"if" => |n| try visitAll(tree, used, &.{ n.condition, n.body }),
        .else_if => |n| try visitAll(tree, used, &.{ n.if1, n.if2 }),
        .@"else" => |n| try visitAll(tree, used, &.{ n.@"if", n.body }),
        .@"switch" => |n| try visitAll(tree, used, &.{ n.expr, n.body }),
        .switch_body => |n| try visitAll(tree, used, &.{ n.attributes, n.clauses }),
        .case_clause => |n| try visitAll(tree, used, &.{ n.selectors, n.body }),
        .case_selector => |n| try visit(tree, used, n.expr),
        .@"while" => |n| try visitAll(tree, used, &.{ n.condition, n.body }),
        .@"return" => |n| try visit(tree, used, n.expr),
        .continuing => |n| try visit(tree, used, n.body),
        .break_if => |n| try visit(tree, used, n.condition),
        .call => |n| try visitAll(tree, used, &.{ n.ident, n.arguments }),
        .@"var" => |n| {
            const extra = tree.extraData(node_mod.Var, n.@"var");
            try visitAll(tree, used, &.{ extra.template_list, extra.name, extra.type, n.initializer });
        },
        // zig fmt off
        .const_assert,
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
        => |n| try visit(tree, used, n.expr),
        .@"=", .@"+=", .@"-=", .@"*=", .@"/=", .@"%=", .@"&=", .@"|=", .@"^=", .@"<<=", .@">>=" => |n| {
            try visitAll(tree, used, &.{ n.lhs_expr, n.rhs_expr });
        },
        .lshift, .rshift => |n| {
            try visitAll(tree, used, &.{ n.lhs_unary_expr, n.rhs_unary_expr });
        },
        .lt, .gt, .lte, .gte, .eq, .neq => |n| {
            try visitAll(tree, used, &.{ n.lhs_shift_expr, n.rhs_shift_expr });
        },
        .mul, .div, .mod => |n| {
            try visitAll(tree, used, &.{ n.lhs_multiplicative_expr, n.rhs_unary_expr });
        },
        .add, .sub => |n| try visitAll(tree, used, &.{ n.lhs_additive_expr, n.rhs_mul_expr }),
        .logical_and, .logical_or => |n| {
            try visitAll(tree, used, &.{ n.lhs_relational_expr, n.rhs_relational_expr });
        },
        .bitwise_and, .bitwise_or, .bitwise_xor => |n| {
            try visitAll(tree, used, &.{ n.lhs_bitwise_expr, n.rhs_unary_expr });
        },
        .field_access => |n| try visit(tree, used, n.lhs_expr),
        .index_access => |n| try visitAll(tree, used, &.{ n.lhs_expr, n.index_expr }),
        .diagnostic_directive, .enable_directive, .requires_directive, .import_alias => {},
        .import, .number, .true, .false, .@"break", .@"continue", .discard, .@"error" => {},
    }
}

pub const Options = struct {
    symbols: [][]const u8 = &.{},
    find_symbols: bool = true,
};

/// Tree shakes globals by removing unused nodes (including imports) from the root span.
/// Tree shakes imports by removing unused alises from their span.
pub fn treeShake(allocator: Allocator, tree: *Ast, opts: Options) Allocator.Error!void {
    var used = Used.init(allocator);
    defer used.deinit();
    for (opts.symbols) |s| try used.put(s, {});
    if (opts.find_symbols) try findSymbols(&used, tree);

    // Make a copy of roots because we will modify them.
    const og_roots = try allocator.dupe(Node.Index, tree.spanToList(0));
    defer allocator.free(og_roots);

    // Find all used identifiers
    for (og_roots) |i| {
        switch (tree.node(i)) {
            .global_var, .override, .@"const", .@"struct", .@"fn", .type_alias => {
                if (used.get(tree.globalName(i))) |_| try visit(tree, &used, i);
            },
            else => {},
        }
    }

    for (og_roots, 0..) |ni, i| {
        switch (tree.node(ni)) {
            // Set unused root span nodes to empty.
            .global_var, .override, .@"const", .@"struct", .@"fn", .type_alias => {
                if (used.get(tree.globalName(ni)) == null) tree.removeFromSpan(0, i);
            },
            // Set unused import nodes to empty.
            .import => |n| {
                var import_used = false;
                if (n.aliases != 0) {
                    for (tree.spanToList(n.aliases)) |j| {
                        const alias = tree.node(j).import_alias;
                        if (used.get(tree.identifier(alias.old))) |_| {
                            import_used = true;
                        } else {
                            tree.removeFromSpan(n.aliases, j);
                        }
                    }
                } else {
                    import_used = true;
                }
                if (!import_used) tree.removeFromSpan(0, i);
            },
            else => {},
        }
    }
}
