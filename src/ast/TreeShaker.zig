const std = @import("std");
const Ast = @import("./Ast.zig");
const Node = @import("./Node.zig");

const Allocator = std.mem.Allocator;
pub const Symbols = std.AutoArrayHashMap(Node.IdentIndex, void);

pub fn entrypoints(allocator: Allocator, tree: Ast) !Symbols {
    var res = Symbols.init(allocator);
    for (tree.spanToList(0)) |i| {
        const node = tree.node(i);
        switch (node.tag) {
            .@"fn" => {
                const header = tree.extraData(Node.FnHeader, node.lhs);
                if (header.attrs == 0) continue;
                for (tree.spanToList(header.attrs)) |a| {
                    const attr_node = tree.node(a);
                    const attr: Node.Attribute = @enumFromInt(attr_node.lhs);
                    switch (attr) {
                        .vertex, .fragment, .compute => {
                            try res.put(header.name, {});
                            break;
                        },
                        else => {},
                    }
                }
            },
            else => {},
        }
    }
    return res;
}

fn addUsedSymbol(tree: *Ast, used: *Symbols, ident: Node.IdentIndex) Allocator.Error!void {
    if (ident == 0 or used.get(ident) != null) return;
    try used.put(ident, {});
    // Cascade
    for (tree.spanToList(0)) |i| {
        switch (tree.node(i).tag) {
            .@"var", .override, .@"const", .@"struct", .@"fn", .type_alias => {
                if (tree.globalIdent(i) == ident) try visit(tree, used, i);
            },
            else => {},
        }
    }
}

fn visitAll(tree: *Ast, used: *Symbols, nodes: []const Node.Index) Allocator.Error!void {
    for (nodes) |n| try visit(tree, used, n);
}

fn visit(tree: *Ast, used: *Symbols, index: Node.Index) Allocator.Error!void {
    if (index == 0) return;
    const node = tree.node(index);
    switch (node.tag) {
        .span => for (tree.spanToList(index)) |i| try visit(tree, used, i),
        .global_var, .@"var" => {
            const v = tree.extraData(Node.Var, node.lhs);
            try visitAll(tree, used, &.{ v.attrs, v.type, node.rhs });
        },
        .override => {
            const override = tree.extraData(Node.Override, node.lhs);
            try visitAll(tree, used, &.{
                override.attrs,
                override.type,
                node.rhs,
            });
        },
        .@"fn" => {
            const header = tree.extraData(Node.FnHeader, node.lhs);
            try visitAll(tree, used, &.{
                header.attrs,
                header.params,
                header.return_attrs,
                header.return_type,
                node.rhs,
            });
        },
        .@"const", .let => {
            const typed_ident = tree.extraData(Node.TypedIdent, node.lhs);
            try visitAll(tree, used, &.{
                typed_ident.name,
                typed_ident.type,
                node.rhs,
            });
        },
        .type_alias => try visit(tree, used, node.rhs),
        .@"struct" => try visit(tree, used, node.rhs),
        .struct_member => {
            const typed_ident = tree.extraData(Node.TypedIdent, node.rhs);
            try visitAll(tree, used, &.{
                node.lhs,
                typed_ident.type,
            });
        },
        .fn_param => {
            const param = tree.extraData(Node.FnParam, node.rhs);
            try visitAll(tree, used, &.{
                node.lhs,
                param.type,
            });
        },
        .type, .ident, .var_ref => {
            try addUsedSymbol(tree, used, node.lhs);
            try visit(tree, used, node.rhs);
        },
        .attribute => {
            const attr: Node.Attribute = @enumFromInt(node.lhs);
            switch (attr) {
                .compute, .@"const", .fragment, .invariant, .must_use, .vertex, .diagnostic, .interpolate => {},
                .@"align", .binding, .builtin, .group, .id, .location, .size => {
                    try visit(tree, used, node.rhs);
                },
                .workgroup_size => {
                    const workgroup_size = tree.extraData(Node.WorkgroupSize, node.rhs);
                    try visitAll(tree, used, &.{
                        workgroup_size.x,
                        workgroup_size.y,
                        workgroup_size.z,
                    });
                },
            }
        },
        .@"for" => {
            const header = tree.extraData(Node.ForHeader, node.lhs);
            try visitAll(tree, used, &.{
                header.attrs,
                header.init,
                header.cond,
                header.update,
                node.rhs,
            });
        },
        .break_if,
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
        => {
            std.debug.assert(node.rhs == 0);
            try visit(tree, used, node.lhs);
        },
        // lhs rhs
        .loop,
        .compound,
        .@"if",
        .else_if,
        .@"else",
        .@"switch",
        .switch_body,
        .case_clause,
        .case_selector,
        .@"while",
        .@"return",
        .continuing,
        .call,
        .@"=",
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
        .lshift,
        .rshift,
        .lt,
        .gt,
        .lte,
        .gte,
        .eq,
        .neq,
        .mul,
        .div,
        .mod,
        .add,
        .sub,
        .logical_and,
        .logical_or,
        .bitwise_and,
        .bitwise_or,
        .bitwise_xor,
        .index_access,
        => try visitAll(tree, used, &.{ node.lhs, node.rhs }),
        .field_access => try visit(tree, used, node.lhs),
        .diagnostic_directive, .enable_directive, .requires_directive, .import_alias => {},
        .import, .true, .false, .@"break", .@"continue", .discard, .comment => {},
        .abstract_int, .i32, .u32, .abstract_float, .f32, .f16 => {},
    }
}

/// Tree shakes globals by removing unused nodes (including imports) from the root span.
/// Tree shakes imports by removing unused aliases from their import span.
pub fn treeShake(allocator: Allocator, tree: *Ast, roots: []const []const u8) Allocator.Error!void {
    var used = Symbols.init(allocator);
    defer used.deinit();
    for (roots) |r| {
        var found = false;
        for (tree.spanToList(0)) |i| {
            const ident = tree.globalIdent(i);
            const name = tree.identifier(ident);
            if (std.mem.eql(u8, r, name)) {
                try used.put(ident, {});
                found = true;
                break;
            }
        }
        if (!found) {
            std.debug.print("warning: could not find entry symbol {s}\n", .{r});
        }
    }

    // Make a copy of root nodes because we may modify them.
    const og_roots = try allocator.dupe(Node.Index, tree.spanToList(0));
    defer allocator.free(og_roots);

    // Find all used identifiers
    for (og_roots) |r| {
        switch (tree.node(r).tag) {
            .@"var", .override, .@"const", .@"struct", .@"fn", .type_alias => {
                if (used.get(tree.globalIdent(r))) |_| try visit(tree, &used, r);
            },
            else => {},
        }
    }

    for (0..og_roots.len) |k| {
        const i = og_roots.len - k - 1;
        const r = og_roots[i];
        const node = tree.node(r);

        switch (node.tag) {
            // Set unused root span nodes to empty.
            .@"var", .override, .@"const", .@"struct", .@"fn", .type_alias => {
                if (used.get(tree.globalIdent(r)) == null) tree.removeFromSpan(0, i);
            },
            // Set unused import nodes to empty.
            .import => {
                const aliases = tree.modAliases(r);
                var import_used = aliases.len == 0;
                for (aliases, 0..) |a, j| {
                    const alias = tree.node(a);
                    if (used.get(alias.lhs)) |_| {
                        import_used = true;
                    } else {
                        tree.removeFromSpan(node.lhs, j);
                    }
                }
                if (!import_used) tree.removeFromSpan(0, i);
            },
            else => {},
        }
    }
}
