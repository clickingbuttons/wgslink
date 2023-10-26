// Rather than lower to IR we'll raise back to WGSL
const Ast = @import("./shader/Ast.zig");
const std = @import("std");

const NodeIndex = Ast.NodeIndex;
const TokenIndex = Ast.TokenIndex;
const Node = Ast.Node;
const Self = @This();

pub const InstIndex = enum(u32) { none = std.math.maxInt(u32), _ };

tree: *const Ast,
tab: [:0]const u8,

pub fn writeTranslationUnit(self: Self, writer: anytype) !void {
    const global_nodes = self.tree.spanToList(.globals);

    for (global_nodes) |node| {
        const tag = self.tree.nodeTag(node);
        try switch (tag) {
            .global_var => self.writeGlobalVar(writer, node),
            .override => self.writeOverride(writer, node),
            .@"const" => self.writeConst(writer, node),
            .@"struct" => self.writeStruct(writer, node),
            .@"fn" => self.writeFn(writer, node),
            .type_alias => self.writeTypeAlias(writer, node),
            .comment => self.writeComment(writer, node),
            else => |t| {
                std.debug.print("could not write node {s}\n", .{@tagName(t)});
            },
        };
        switch (tag) {
            .global_var, .override, .@"const", .type_alias => try writer.writeAll(";"),
            else => {},
        }

        try writer.writeAll("\n");
    }
}

fn writeVar(self: Self, writer: anytype, extra: anytype, node: NodeIndex) !void {
    try writer.writeAll("var");

    if (extra.addr_space != .none) {
        const loc = self.tree.tokenLoc(extra.addr_space);
        try writer.writeAll("<");
        try writer.writeAll(loc.slice(self.tree.source));
    }

    if (extra.access_mode != .none) {
        const loc = self.tree.tokenLoc(extra.access_mode);
        try writer.writeAll(",");
        try writer.writeAll(loc.slice(self.tree.source));
    }

    if (extra.addr_space != .none) try writer.writeAll(">");

    try writer.writeAll(" ");
    try self.writeNodeName(writer, node);

    if (extra.type != .none) {
        try writer.writeAll(": ");
        try self.writeType(writer, extra.type);
    }

    const rhs = self.tree.nodeRHS(node);
    if (rhs != .none) {
        try writer.writeAll(" = ");
        try self.writeExpr(writer, rhs);
    }
}

fn writeGlobalVar(self: Self, writer: anytype, node: NodeIndex) !void {
    const extra = self.tree.extraData(Node.GlobalVar, self.tree.nodeLHS(node));

    if (extra.attrs != NodeIndex.none) {
        for (self.tree.spanToList(extra.attrs)) |attr| {
            switch (self.tree.nodeTag(attr)) {
                .attr_group => {
                    try writer.writeAll("@group(");
                    try self.writeExpr(writer, self.tree.nodeLHS(attr));
                    try writer.writeAll(") ");
                },
                .attr_binding => {
                    try writer.writeAll("@binding(");
                    try self.writeExpr(writer, self.tree.nodeLHS(attr));
                    try writer.writeAll(") ");
                },
                else => {},
            }
        }
    }

    try self.writeVar(writer, extra, node);
}

fn writeType(self: Self, writer: anytype, node: NodeIndex) !void {
    const lhs = self.tree.nodeLHS(node);
    const rhs = self.tree.nodeRHS(node);
    try self.writeNode(writer, node);
    switch (self.tree.nodeTag(node)) {
        .atomic_type,
        .array_type,
        .vector_type,
        .matrix_type,
        .sampled_texture_type,
        .multisampled_texture_type,
        => {
            if (lhs != .none) {
                try writer.writeAll("<");
                try self.writeType(writer, lhs);
            }
            if (rhs != .none) {
                try writer.writeAll(",");
                try self.writeType(writer, rhs);
            }
            if (lhs != .none) try writer.writeAll(">");
        },
        .storage_texture_type => {
            try writer.writeAll("<");
            const tok1 = self.tree.nodeLHS(node).asTokenIndex();
            try writer.writeAll(self.tree.tokenLoc(tok1).slice(self.tree.source));
            try writer.writeAll(",");
            const tok2 = self.tree.nodeRHS(node).asTokenIndex();
            try writer.writeAll(self.tree.tokenLoc(tok2).slice(self.tree.source));
            try writer.writeAll(">");
        },
        .ptr_type => {
            const extra = self.tree.extraData(Node.PtrType, self.tree.nodeRHS(node));
            try writer.writeAll("<");
            try writer.writeAll(self.tree.tokenLoc(extra.addr_space).slice(self.tree.source));
            try writer.writeAll(",");
            try self.writeType(writer, self.tree.nodeLHS(node));

            if (extra.access_mode != .none) {
                try writer.writeAll(",");
                try writer.writeAll(self.tree.tokenLoc(extra.access_mode).slice(self.tree.source));
            }
            try writer.writeAll(">");
        },
        else => {},
    }
}

fn writeConst(self: Self, writer: anytype, node: NodeIndex) !void {
    const lhs = self.tree.nodeLHS(node);
    const rhs = self.tree.nodeRHS(node);
    const name = self.tree.declNameLoc(node).?;
    try self.writeNode(writer, node);
    try writer.writeAll(" ");
    try writer.writeAll(name.slice(self.tree.source));
    if (lhs != .none) {
        try writer.writeAll(": ");
        try self.writeType(writer, lhs);
    }
    try writer.writeAll(" = ");
    try self.writeExpr(writer, rhs);
}

fn writeExpr(self: Self, writer: anytype, node: NodeIndex) !void {
    const tag = self.tree.nodeTag(node);
    const lhs = self.tree.nodeLHS(node);
    const rhs = self.tree.nodeRHS(node);
    switch (tag) {
        .number,
        .ident,
        => try self.writeNode(writer, node),
        .true => try writer.writeAll("true"),
        .false => try writer.writeAll("false"),
        .not => {
            try writer.writeAll("!(");
            try self.writeExpr(writer, lhs);
            try writer.writeAll(")");
        },
        .negate => {
            try writer.writeAll("-");
            try self.writeExpr(writer, lhs);
        },
        .deref => {
            try writer.writeAll(".");
            try self.writeExpr(writer, lhs);
        },
        .addr_of => {
            try writer.writeAll("&");
            try self.writeExpr(writer, lhs);
        },
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
        => |t| {
            try self.writeExpr(writer, lhs);
            try writer.writeAll(switch (t) {
                .mul => "*",
                .div => "/",
                .mod => "%",
                .add => "+",
                .sub => "-",
                .shl => "<<",
                .shr => ">>",
                .@"and" => "&",
                .@"or" => "|",
                .xor => "^",
                .logical_and => "&&",
                .logical_or => "||",
                .equal => "=",
                .not_equal => "!=",
                .less_than => "<",
                .less_than_equal => "<=",
                .greater_than => ">",
                .greater_than_equal => ">=",
                else => "",
            });
            try self.writeExpr(writer, rhs);
        },
        .index_access => {
            try self.writeExpr(writer, lhs);
            try writer.writeAll("[");
            try self.writeExpr(writer, rhs);
            try writer.writeAll("]");
        },
        .field_access => {
            try self.writeExpr(writer, lhs);
            try writer.writeAll(".");
            try self.writeToken(writer, rhs.asTokenIndex());
        },
        .call => {
            try self.writeCall(@TypeOf(writer).Error, writer, node);
        },
        .bitcast => {
            try self.writeNode(writer, lhs);
            try writer.writeAll("(");
            try self.writeExpr(writer, rhs);
            try writer.writeAll(")");
        },
        else => |t| {
            std.debug.print("invalid expression {s}\n", .{@tagName(t)});
        },
    }
}

fn writeAttributes(self: Self, writer: anytype, attrs: NodeIndex) !void {
    for (self.tree.spanToList(attrs)) |attr| {
        try writer.writeAll("@");
        const tag = self.tree.nodeTag(attr);
        const attr_name = switch (tag) {
            .attr_const => "const",
            .attr_invariant => "invariant",
            .attr_must_use => "must_use",
            .attr_vertex => "vertex",
            .attr_fragment => "fragment",
            .attr_compute => "compute",
            .attr_align => "align",
            .attr_binding => "binding",
            .attr_group => "group",
            .attr_id => "id",
            .attr_location => "location",
            .attr_size => "size",
            .attr_builtin => "builtin",
            .attr_workgroup_size => "workgroup_size",
            .attr_interpolate => "interpolate",
            else => "UNKNOWN ATTRIBUTE",
        };
        try writer.writeAll(attr_name);
        const lhs = self.tree.nodeLHS(attr);
        if (lhs != .none) {
            try writer.writeAll("(");
            switch (tag) {
                .attr_builtin, .attr_interpolate => try self.writeToken(writer, lhs.asTokenIndex()),
                .attr_workgroup_size => {
                    const extra = self.tree.extraData(Node.WorkgroupSize, lhs);
                    try self.writeExpr(writer, extra.x);
                    if (extra.y != .none) {
                        try writer.writeAll(",");
                        try self.writeExpr(writer, extra.y);
                    }
                    if (extra.z != .none) {
                        try writer.writeAll(",");
                        try self.writeExpr(writer, extra.z);
                    }
                },
                else => try self.writeNode(writer, lhs),
            }

            try writer.writeAll(")");
        }
        try writer.writeAll(" ");
    }
}

fn writeStruct(self: Self, writer: anytype, node: NodeIndex) !void {
    try writer.writeAll("struct ");
    try writer.writeAll(self.tree.declNameLoc(node).?.slice(self.tree.source));
    try writer.writeAll(" {");
    const member_nodes_list = self.tree.spanToList(self.tree.nodeLHS(node));
    for (member_nodes_list) |member_node| {
        const member_name_loc = self.tree.tokenLoc(self.tree.nodeToken(member_node));
        const name = member_name_loc.slice(self.tree.source);
        try writer.writeAll("\n");
        try writer.writeAll(self.tab);
        const member_attrs_node = self.tree.nodeLHS(member_node);
        if (member_attrs_node != .none) try self.writeAttributes(writer, member_attrs_node);
        try writer.print("{s}: ", .{name});
        try self.writeType(writer, self.tree.nodeRHS(member_node));
        try writer.writeAll(",");
    }
    try writer.writeAll("\n}");
}

fn writeNodeName(self: Self, writer: anytype, node: NodeIndex) !void {
    try writer.writeAll(self.tree.declNameLoc(node).?.slice(self.tree.source));
}

fn writeNode(self: Self, writer: anytype, node: NodeIndex) !void {
    try writer.writeAll(self.tree.nodeLoc(node).slice(self.tree.source));
}

fn writeToken(self: Self, writer: anytype, tok: TokenIndex) !void {
    try writer.writeAll(self.tree.tokenLoc(tok).slice(self.tree.source));
}

fn writeCall(self: Self, comptime Error: type, writer: anytype, node: NodeIndex) Error!void {
    const ty = self.tree.nodeRHS(node);
    if (ty != .none) try self.writeType(writer, ty) else try self.writeNode(writer, node);

    try writer.writeAll("(");
    const args = self.tree.nodeLHS(node);
    if (args != .none) {
        const list = self.tree.spanToList(args);
        for (list, 0..) |arg, i| {
            try self.writeExpr(writer, arg);
            if (i != list.len - 1) try writer.writeAll(", ");
        }
    }
    try writer.writeAll(")");
}

fn writeOverride(self: Self, writer: anytype, node: NodeIndex) !void {
    const extra = self.tree.extraData(Node.Override, self.tree.nodeLHS(node));

    if (extra.attrs != .none) try self.writeAttributes(writer, extra.attrs);

    try writer.writeAll("override ");
    try self.writeNodeName(writer, node);

    if (extra.type != .none) {
        try writer.writeAll(": ");
        try self.writeType(writer, extra.type);
    }

    const rhs = self.tree.nodeRHS(node);
    if (rhs != .none) {
        try writer.writeAll(" = ");
        try self.writeExpr(writer, rhs);
    }
}

fn writeTypeAlias(self: Self, writer: anytype, node: NodeIndex) !void {
    try writer.writeAll("alias ");
    try self.writeNodeName(writer, node);
    try writer.writeAll(" = ");
    try self.writeNode(writer, self.tree.nodeLHS(node));
}

fn writeStatement(self: Self, writer: anytype, node: NodeIndex, depth: usize) !bool {
    for (0..depth) |_| try writer.writeAll(self.tab);
    const tag = self.tree.nodeTag(node);
    switch (tag) {
        .compound_assign => {
            try self.writeExpr(writer, self.tree.nodeLHS(node));
            try writer.writeAll(" = ");
            try self.writeExpr(writer, self.tree.nodeRHS(node));
        },
        .phony_assign => {
            try writer.writeAll("_ = ");
            try self.writeExpr(writer, self.tree.nodeLHS(node));
        },
        // .call => try astgen.genCall(scope, node),
        .@"return" => {
            try writer.writeAll("return ");
            try self.writeExpr(writer, self.tree.nodeLHS(node));
        },
        .comment => try self.writeComment(writer, node),
        // .break_if => try astgen.genBreakIf(scope, node),
        // .@"if" => try astgen.genIf(scope, node),
        // .if_else => try astgen.genIfElse(scope, node),
        // .if_else_if => try astgen.genIfElseIf(scope, node),
        // .@"while" => try astgen.genWhile(scope, node),
        // .@"for" => try astgen.genFor(scope, node),
        // .@"switch" => try astgen.genSwitch(scope, node),
        // .loop => try astgen.genLoop(scope, node),
        // .block => blk: {
        //     var inner_scope = try astgen.scope_pool.create();
        //     inner_scope.* = .{ .tag = .block, .parent = scope };
        //     const inner_block = try astgen.genBlock(inner_scope, node);
        //     break :blk inner_block;
        // },
        // .continuing => try astgen.genContinuing(scope, node),
        // .discard => try astgen.addInst(.discard),
        // .@"break" => try astgen.addInst(.@"break"),
        // .@"continue" => try astgen.addInst(.@"continue"),
        // .increase => try astgen.genIncreaseDecrease(scope, node, .add),
        // .decrease => try astgen.genIncreaseDecrease(scope, node, .sub),
        .@"const", .let => try self.writeConst(writer, node),
        .@"var" => {
            const extra = self.tree.extraData(Node.Var, self.tree.nodeLHS(node));
            try self.writeVar(writer, extra, node);
        },
        // .@"const" => blk: {
        //     const decl = try astgen.genConst(scope, node);
        //     scope.decls.putAssumeCapacity(node, decl);
        //     break :blk decl;
        // },
        // .let => blk: {
        //     const decl = try astgen.genLet(scope, node);
        //     scope.decls.putAssumeCapacity(node, decl);
        //     break :blk decl;
        // },
        else => {},
    }

    switch (tag) {
        .comment => {},
        else => try writer.writeAll(";"),
    }
    try writer.writeAll("\n");

    return false;
}

fn writeBlock(self: Self, writer: anytype, node: NodeIndex, depth: usize) !void {
    const lhs = self.tree.nodeLHS(node);

    try writer.writeAll("{");

    if (lhs != .none) {
        const statements = self.tree.spanToList(lhs);
        if (statements.len > 0) try writer.writeAll("\n");
        for (statements) |n| {
            if (try self.writeStatement(writer, n, depth)) return;
        }
    }

    try writer.writeAll("}");
}

fn writeFn(self: Self, writer: anytype, node: NodeIndex) !void {
    const extra = self.tree.extraData(Node.FnProto, self.tree.nodeLHS(node));

    if (extra.attrs != .none) try self.writeAttributes(writer, extra.attrs);

    try writer.writeAll("fn ");
    try self.writeNodeName(writer, node);
    try writer.writeAll("(");

    if (extra.params != .none) {
        const params = self.tree.spanToList(extra.params);
        for (params, 0..) |p, i| {
            const attrs = self.tree.nodeLHS(p);
            if (attrs != .none) try self.writeAttributes(writer, attrs);

            try self.writeNode(writer, p);
            try writer.writeAll(": ");
            try self.writeType(writer, self.tree.nodeRHS(p));
            if (i != params.len - 1) try writer.writeAll(",");
        }
    }
    try writer.writeAll(")");
    if (extra.return_type != .none) {
        try writer.writeAll(" -> ");
        if (extra.return_attrs != .none) try self.writeAttributes(writer, extra.return_attrs);
        try self.writeType(writer, extra.return_type);
    }
    try writer.writeAll(" ");
    try self.writeBlock(writer, self.tree.nodeRHS(node), 1);
}

fn writeComment(self: Self, writer: anytype, node: NodeIndex) !void {
    try self.writeNode(writer, node);
}
