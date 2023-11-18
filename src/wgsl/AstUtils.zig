const std = @import("std");
const Ast = @import("./Ast.zig");
const Node = @import("./Node.zig");
const Token = @import("./Token.zig");
const Data = Node.Data;

const max_span_len = 100;

pub const VisitData = union(enum) {
    node: Node.Index,
    token: Token.Index,
    token_tag: Token.Tag, // For required syntax
    span_start: Token.Index,
    span_sep: Token.Index,
    span_end: Token.Index,
};

fn visitNodeList(ctx: anytype, span_start: Token.Index, span: []const Node.Index) @typeInfo(@TypeOf(ctx.visitor)).Fn.return_type.? {
    try ctx.visitor(ctx, VisitData{ .span_start = span_start });
    var filtered = std.BoundedArray(Node.Index, max_span_len){};
    for (span) |s| if (s != 0) filtered.appendAssumeCapacity(s);
    for (filtered.slice(), 0..) |n, i| {
        try visit(ctx, n);
        if (i != span.len - 1) try ctx.visitor(ctx, VisitData{ .span_sep = span_start });
    }
    try ctx.visitor(ctx, VisitData{ .span_end = span_start });
}

fn visitTokenList(ctx: anytype, span_start: Token.Index, span: []const Token.Index) @typeInfo(@TypeOf(ctx.visitor)).Fn.return_type.? {
    try ctx.visitor(ctx, VisitData{ .span_start = span_start });
    for (span, 0..) |t, i| {
        try ctx.visitor(ctx, VisitData{ .token = t });
        if (i != span.len - 1) try ctx.visitor(ctx, VisitData{ .span_sep = span_start });
    }
    try ctx.visitor(ctx, VisitData{ .span_end = span_start });
}

fn visitDiagnosticControl(ctx: anytype, span_start: Token.Index, node: Node.ExtraIndex) @typeInfo(@TypeOf(ctx.visitor)).Fn.return_type.? {
    const tree: Ast = ctx.tree;
    const visitor = ctx.visitor;
    const control = tree.extraData(Node.DiagnosticControl, node);
    try visitor(ctx, VisitData{ .span_start = span_start });
    try visitor(ctx, VisitData{ .token = control.severity });
    try visitor(ctx, VisitData{ .span_sep = span_start });
    try visitor(ctx, VisitData{ .token = control.name });
    if (control.field != 0) {
        try visitor(ctx, VisitData{ .token_tag = .@"." });
        try visitor(ctx, VisitData{ .token = control.field });
    }
    try visitor(ctx, VisitData{ .span_end = span_start });
}

fn visitInitializer(ctx: anytype, initializer: Node.Index) @typeInfo(@TypeOf(ctx.visitor)).Fn.return_type.? {
    if (initializer == 0) return;
    try ctx.visitor(ctx, VisitData{ .token_tag = .@"=" });
    try visit(ctx, initializer);
}

/// Visits nodes in source order.
pub fn visit(ctx: anytype, node: Node.Index) @typeInfo(@TypeOf(ctx.visitor)).Fn.return_type.? {
    const tree: Ast = ctx.tree;
    const visitor = ctx.visitor;
    const token = tree.nodeToken(node);
    const tag = tree.nodeTag(node);
    switch (tag) {
        .empty => {},
        .span => try visitNodeList(ctx, token, tree.spanToList(node)),
        // Directives
        .diagnostic_directive => {
            const data = tree.nodeData(Data.DiagnosticDirective, node);
            try visitor(ctx, VisitData{ .node = node });
            try visitDiagnosticControl(ctx, token + 1, data.control);
        },
        .enable_directive => {
            const data = tree.nodeData(Data.EnableDirective, node);
            try visitor(ctx, VisitData{ .node = node });
            try visitTokenList(ctx, token, tree.spanToList(data.ident_tokens));
        },
        .requires_directive => {
            const data = tree.nodeData(Data.RequiresDirective, node);
            try visitor(ctx, VisitData{ .node = node });
            try visitTokenList(ctx, token, tree.spanToList(data.ident_tokens));
        },
        // Global declarations
        .global_var => {
            const data = tree.nodeData(Data.GlobalVar, node);
            const global_var = tree.extraData(Node.GlobalVar, data.global_var);
            if (global_var.attrs != 0) try visit(ctx, global_var.attrs);
            try visitor(ctx, VisitData{ .node = node });
            if (global_var.template_list != 0) try visit(ctx, global_var.template_list);
            try visitor(ctx, VisitData{ .token = global_var.name });
            if (global_var.type != 0) try visit(ctx, global_var.type);
            try visitInitializer(ctx, data.initializer);
        },
        .override => {
            const data = tree.nodeData(Data.Override, node);
            const override = tree.extraData(Node.Override, data.override);
            if (override.attrs != 0) try visit(ctx, override.attrs);
            try visitor(ctx, VisitData{ .node = node });
            try visitor(ctx, VisitData{ .token = token + 1 });
            if (override.type != 0) try visit(ctx, override.type);
            try visitInitializer(ctx, data.initializer);
        },
        .@"fn" => {
            const data = tree.nodeData(Data.Fn, node);
            const header = tree.extraData(Node.FnHeader, data.fn_header);
            if (header.attrs != 0) try visit(ctx, header.attrs);
            try visitor(ctx, VisitData{ .node = node }); // fn
            try visitor(ctx, VisitData{ .token = token + 1 }); // main
            const param_list = tree.spanToList(if (header.params == 0) null else header.params);
            try visitNodeList(ctx, token + 2, param_list);
            if (header.return_attrs != 0 or header.return_type != 0) {
                try visitor(ctx, VisitData{ .token_tag = .@"->" });
            }
            if (header.return_attrs != 0) try visit(ctx, header.return_attrs);
            if (header.return_type != 0) try visit(ctx, header.return_type);
            try visit(ctx, data.body);
        },
        .@"const" => {
            const data = tree.nodeData(Data.Const, node);
            try visitor(ctx, VisitData{ .node = node });
            try visitor(ctx, VisitData{ .token = tree.nodeToken(node) + 1 });
            if (data.type != 0) try visit(ctx, data.type);
            try visitInitializer(ctx, data.initializer);
        },
        .type_alias => {
            const data = tree.nodeData(Data.TypeAlias, node);
            try visitor(ctx, VisitData{ .node = node });
            try visitor(ctx, VisitData{ .token = tree.nodeToken(node) + 1 });
            try visit(ctx, data.value);
        },
        .import => {
            const data = tree.nodeData(Data.Import, node);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.aliases);
            try visitor(ctx, VisitData{ .token = tree.nodeToken(data.module) });
        },
        .@"struct" => {
            const data = tree.nodeData(Data.Struct, node);
            try visitor(ctx, VisitData{ .node = node });
            try visitor(ctx, VisitData{ .token = token + 1 });
            try visitNodeList(ctx, token, tree.spanToList(data.members));
        },
        // Global declaration helpers
        .attr => {
            const attr = tree.nodeData(Data.Attr, node);
            try visitor(ctx, VisitData{ .node = node });
            const span_start = token + 2;
            switch (attr.tag) {
                .compute, .@"const", .fragment, .invariant, .must_use, .vertex => {},
                inline .@"align", .binding, .builtin, .group, .id, .location, .size => |t| {
                    const single_expr = @field(attr.data, @tagName(t));
                    try visitNodeList(ctx, span_start, &.{single_expr});
                },
                .diagnostic => {
                    const diag: Node.Attribute.DiagnosticControl = attr.data.diagnostic;
                    try visitDiagnosticControl(ctx, span_start, diag);
                },
                .interpolate => {
                    const interpolate: Node.Attribute.Interpolate = attr.data.interpolate;
                    const interpolation = tree.extraData(Node.Interpolation, interpolate);
                    try visitNodeList(ctx, span_start, &.{
                        interpolation.type,
                        interpolation.sampling_expr,
                    });
                },
                .workgroup_size => {
                    const workgroup_size: Node.Attribute.WorkgroupSize = attr.data.workgroup_size;
                    const size = tree.extraData(Node.WorkgroupSize, workgroup_size);
                    try visitNodeList(ctx, span_start, &.{ size.x, size.y, size.z });
                },
            }
        },
        .ident => {
            const data = tree.nodeData(Data.Ident, node);
            try visitor(ctx, VisitData{ .token = tree.nodeToken(node) });
            if (data.template_list != 0) try visit(ctx, data.template_list);
        },
        .type => {
            const data = tree.nodeData(Data.Type, node);
            const prev_tok = tree.nodeToken(node) - 1;
            if (tree.tokenTag(prev_tok) == .@":") try visitor(ctx, VisitData{ .token_tag = .@":" });
            try visitor(ctx, VisitData{ .node = node });
            if (data.template_list != 0) try visit(ctx, data.template_list);
        },
        .alias => {
            const data = tree.nodeData(Data.Alias, node);
            try visitor(ctx, VisitData{ .node = node });
            try visitor(ctx, VisitData{ .token = tree.nodeToken(node) + 1 });
            try visit(ctx, data.alias);
        },
        .struct_member => {
            const data = tree.nodeData(Data.StructMember, node);
            if (data.attributes != 0) try visit(ctx, data.attributes);
            try visitor(ctx, VisitData{ .token = tree.nodeToken(node) });
            try visit(ctx, data.type);
        },
        .fn_param => {
            const data = tree.nodeData(Data.FnParam, node);
            if (data.attributes != 0) try visit(ctx, data.attributes);
            try visitor(ctx, VisitData{ .token = tree.nodeToken(node) });
            try visit(ctx, data.type);
        },
        // Statements
        .loop => {
            const data = tree.nodeData(Data.Loop, node);
            if (data.attributes != 0) try visit(ctx, data.attributes);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.body);
        },
        .compound => {
            const data = tree.nodeData(Data.Compound, node);
            if (data.attributes != 0) try visit(ctx, data.attributes);
            try visitor(ctx, VisitData{ .node = node });
            const end_tok = if (data.statements != 0) brk: {
                try visit(ctx, data.statements);
                const nodes = tree.spanToList(node);
                const last_statement = nodes[nodes.len - 1];
                break :brk tree.nodeToken(last_statement) + 1;
            } else tree.nodeToken(node) + 1;
            try visitor(ctx, VisitData{ .token = end_tok });
        },
        .@"for" => {
            const data = tree.nodeData(Data.For, node);
            const header = tree.extraData(Node.ForHeader, data.for_header);
            if (header.attrs != 0) try visit(ctx, header.attrs);
            try visitor(ctx, VisitData{ .node = node });
            if (header.init != 0) try visit(ctx, header.init);
            if (header.cond != 0) try visit(ctx, header.cond);
            if (header.update != 0) try visit(ctx, header.update);
            try visit(ctx, data.body);
        },
        .@"if" => {
            const data = tree.nodeData(Data.If, node);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.condition);
            try visit(ctx, data.body);
        },
        .@"switch" => {
            const data = tree.nodeData(Data.Switch, node);
            if (data.expr != 0) try visit(ctx, data.expr);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.body);
        },
        .@"while" => {
            const data = tree.nodeData(Data.While, node);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.condition);
            try visit(ctx, data.body);
        },
        .@"return" => {
            const data = tree.nodeData(Data.Return, node);
            try visitor(ctx, VisitData{ .node = node });
            if (data.expr != 0) try visit(ctx, data.expr);
        },
        .call => {
            const data = tree.nodeData(Data.Call, node);
            try visit(ctx, data.ident);
            if (data.arguments != 0)
                try visit(ctx, data.arguments)
            else {
                try visitor(ctx, VisitData{ .token = tree.nodeToken(node) + 1 }); // (
                try visitor(ctx, VisitData{ .token = tree.nodeToken(node) + 2 }); // )
            }
        },
        .@"var" => {
            const data = tree.nodeData(Data.Var, node);
            const extra = tree.extraData(Node.Var, data.@"var");
            try visitor(ctx, VisitData{ .node = node });
            if (extra.template_list != 0) try visit(ctx, extra.template_list);
            if (extra.type != 0) try visit(ctx, extra.type);
            try visitor(ctx, VisitData{ .token = tree.nodeToken(extra.name) });
            try visitInitializer(ctx, data.initializer);
        },
        .let => {
            const data = tree.nodeData(Data.Let, node);
            try visitor(ctx, VisitData{ .node = node });
            if (data.type != 0) try visit(ctx, data.type);
            try visitInitializer(ctx, data.initializer);
        },
        .@"break", .@"continue", .discard => try visitor(ctx, VisitData{ .node = node }),
        .variable_updating => {
            const data = tree.nodeData(Data.VariableUpdating, node);
            if (data.lhs_expr != 0) try visit(ctx, data.lhs_expr);
            try visitor(ctx, VisitData{ .node = node });
            if (data.rhs_expr != 0) try visit(ctx, data.rhs_expr);
        },
        .const_assert => {
            const data = tree.nodeData(Data.ConstAssert, node);
            try visitor(ctx, VisitData{ .node = node });
            if (data.expr != 0) try visit(ctx, data.expr);
        },
        .continuing => {
            const data = tree.nodeData(Data.Continuing, node);
            try visitor(ctx, VisitData{ .node = node });
            if (data.body != 0) try visit(ctx, data.body);
        },
        .break_if => {
            const data = tree.nodeData(Data.BreakIf, node);
            try visitor(ctx, VisitData{ .node = node });
            if (data.condition != 0) try visit(ctx, data.condition);
        },
        // Statement helpers
        .@"else" => {
            const data = tree.nodeData(Data.Else, node);
            try visit(ctx, data.@"if");
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.body);
        },
        .else_if => {
            const data = tree.nodeData(Data.ElseIf, node);
            try visit(ctx, data.if1);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.if2);
        },
        .switch_body => {
            const data = tree.nodeData(Data.SwitchBody, node);
            if (data.attributes != 0) try visit(ctx, data.attributes);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.clauses);
        },
        .case_clause => {
            const data = tree.nodeData(Data.CaseClause, node);
            try visitor(ctx, VisitData{ .node = node });
            if (data.selectors != 0) try visit(ctx, data.selectors);
            try visit(ctx, data.body);
        },
        .case_selector => {
            const data = tree.nodeData(Data.CaseSelector, node);
            // "default"
            if (data.expression == 0) try visitor(ctx, VisitData{ .token = tree.nodeToken(node) }) else try visit(ctx, data.expression);
        },
        // Expressions
        .paren_expr => {
            const data = tree.nodeData(Data.ParenExpr, node);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.expr);
            try visitor(ctx, VisitData{ .token_tag = .@")" });
        },
        .unary_expr => {
            const data = tree.nodeData(Data.UnaryExpr, node);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.expr);
        },
        .shift_expr => {
            const data = tree.nodeData(Data.ShiftExpr, node);
            try visit(ctx, data.lhs_unary_expr);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.rhs_unary_expr);
        },
        .relational_expr => {
            const data = tree.nodeData(Data.RelationalExpr, node);
            try visit(ctx, data.lhs_shift_expr);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.rhs_shift_expr);
        },
        .multiplicative_expr => {
            const data = tree.nodeData(Data.MultiplicativeExpr, node);
            try visit(ctx, data.lhs_multiplicative_expr);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.rhs_unary_expr);
        },
        .additive_expr => {
            const data = tree.nodeData(Data.AdditiveExpr, node);
            try visit(ctx, data.lhs_additive_expr);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.rhs_mul_expr);
        },
        .short_circuit_expr => {
            const data = tree.nodeData(Data.ShortCircuitExpr, node);
            try visit(ctx, data.lhs_relational_expr);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.rhs_relational_expr);
        },
        .bitwise_expr => {
            const data = tree.nodeData(Data.BitwiseExpr, node);
            try visit(ctx, data.lhs_bitwise_expr);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.rhs_unary_expr);
        },
        .lhs_expr => {
            const data = tree.nodeData(Data.LhsExpr, node);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.lhs_expr);
        },
        .field_access => {
            const data = tree.nodeData(Data.FieldAccess, node);
            try visit(ctx, data.lhs_expr);
            try visitor(ctx, VisitData{ .node = node });
            try visitor(ctx, VisitData{ .token = data.member });
        },
        .index_access => {
            const data = tree.nodeData(Data.IndexAccess, node);
            try visit(ctx, data.lhs_expr);
            try visitor(ctx, VisitData{ .node = node });
            try visit(ctx, data.index_expr);
        },
        // Literals
        .true, .false, .number => {
            try visitor(ctx, VisitData{ .token = tree.nodeToken(node) });
        },
    }
}
