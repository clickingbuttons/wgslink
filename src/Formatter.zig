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
    for (self.tree.globals) |node| {
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
    try self.writeNode(writer, node);

    const lhs = self.tree.nodeLHS(node);
    const rhs = self.tree.nodeRHS(node);
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
        .number, .ident, .true, .false => try self.writeNode(writer, node),
        .not, .negate, .deref => {
            try self.writeNode(writer, node);
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
            try writer.writeAll(" ");
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
                .equal => "==",
                .not_equal => "!=",
                .less_than => "<",
                .less_than_equal => "<=",
                .greater_than => ">",
                .greater_than_equal => ">=",
                else => "",
            });
            try writer.writeAll(" ");
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
        .call => try self.writeCall(writer, node),
        .paren_expr => {
            try writer.writeAll("(");
            try self.writeExpr(writer, lhs);
            try writer.writeAll(")");
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
    const members = self.tree.spanToList(self.tree.nodeLHS(node));
    for (members) |m| {
        const member_name_loc = self.tree.tokenLoc(self.tree.nodeToken(m));
        const name = member_name_loc.slice(self.tree.source);
        try writer.writeAll("\n");
        try writer.writeAll(self.tab);
        const member_attrs_node = self.tree.nodeLHS(m);
        if (member_attrs_node != .none) try self.writeAttributes(writer, member_attrs_node);
        try writer.print("{s}: ", .{name});
        try self.writeType(writer, self.tree.nodeRHS(m));
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

fn writeCall(self: Self, writer: anytype, node: NodeIndex) @TypeOf(writer).Error!void {
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

fn writeIf(self: Self, writer: anytype, node: NodeIndex, depth: usize) !void {
    try writer.writeAll("if ");
    try self.writeExpr(writer, self.tree.nodeLHS(node));
    try writer.writeAll(" ");
    try self.writeBlock(writer, self.tree.nodeRHS(node), depth + 1);
}

fn writeStatement(self: Self, writer: anytype, node: NodeIndex, depth: usize) @TypeOf(writer).Error!bool {
    const tag = self.tree.nodeTag(node);
    const lhs = self.tree.nodeLHS(node);
    const rhs = self.tree.nodeRHS(node);

    for (0..depth) |_| try writer.writeAll(self.tab);
    switch (tag) {
        .compound_assign => {
            try self.writeExpr(writer, lhs);
            try writer.writeAll(" = ");
            try self.writeExpr(writer, rhs);
        },
        .phony_assign => {
            try writer.writeAll("_ = ");
            try self.writeExpr(writer, lhs);
        },
        .call => try self.writeCall(writer, node),
        .@"return" => {
            try writer.writeAll("return");
            if (lhs != .none) {
                try writer.writeAll(" ");
                try self.writeExpr(writer, lhs);
            }
            return true;
        },
        .comment => try self.writeComment(writer, node),
        .break_if => {
            try writer.writeAll("break if ");
            try self.writeExpr(writer, lhs);
        },
        .@"if" => try self.writeIf(writer, node, depth),
        .if_else => {
            try self.writeIf(writer, lhs, depth);
            try writer.writeAll(" else ");
            try self.writeBlock(writer, rhs, depth + 1);
        },
        .if_else_if => {
            try self.writeIf(writer, lhs, depth);
            try writer.writeAll(" else ");
            try self.writeIf(writer, rhs, depth);
        },
        .@"while" => {
            try writer.writeAll("while (");
            try self.writeExpr(writer, lhs);
            try writer.writeAll(") ");
            try self.writeBlock(writer, rhs, depth + 1);
        },
        .@"for" => {
            const extra = self.tree.extraData(Node.ForHeader, self.tree.nodeLHS(node));
            try writer.writeAll("for (");
            if (extra.init != .none) _ = try self.writeStatement(writer, extra.init, 0);
            try writer.writeAll("; ");
            if (extra.cond != .none) _ = try self.writeExpr(writer, extra.cond);
            try writer.writeAll("; ");
            if (extra.update != .none) _ = try self.writeStatement(writer, extra.update, 0);
            try writer.writeAll(") ");
            try self.writeBlock(writer, rhs, depth + 1);
        },
        .@"switch" => {
            try writer.writeAll("switch ");
            try self.writeExpr(writer, lhs);
            try writer.writeAll(" {");
            if (rhs != .none) for (self.tree.spanToList(rhs)) |c| {
                try writer.writeAll("\n");
                for (0..depth + 1) |_| try writer.writeAll(self.tab);
                switch (self.tree.nodeTag(c)) {
                    .switch_default, .switch_case_default => try writer.writeAll("default"),
                    .switch_case => {
                        const expr_list = self.tree.nodeLHS(c);
                        if (expr_list != .none) {
                            try writer.writeAll("case ");
                            const expressions = self.tree.spanToList(expr_list);
                            for (expressions, 0..) |e, i| {
                                try self.writeExpr(writer, e);
                                if (i != expressions.len - 1) try writer.writeAll(", ");
                            }
                        }
                    },
                    else => unreachable,
                }
                try writer.writeAll(" ");
                try self.writeBlock(writer, self.tree.nodeRHS(c), depth + 2);
            };
            try writer.writeAll("\n");
            for (0..depth) |_| try writer.writeAll(self.tab);
            try writer.writeAll("}");
        },
        .loop => {
            try writer.writeAll("loop ");
            try self.writeBlock(writer, lhs, depth + 1);
        },
        .block => try self.writeBlock(writer, node, depth + 1),
        .continuing => {
            try writer.writeAll("continuing ");
            try self.writeBlock(writer, lhs, depth + 1);
        },
        .discard => try writer.writeAll("discard"),
        .@"break" => try writer.writeAll("break"),
        .@"continue" => try writer.writeAll("continue"),
        .increase, .decrease => {
            try self.writeExpr(writer, lhs);
            try self.writeNode(writer, node);
        },
        .@"const", .let => try self.writeConst(writer, node),
        .@"var" => {
            const extra = self.tree.extraData(Node.Var, self.tree.nodeLHS(node));
            try self.writeVar(writer, extra, node);
        },
        else => {
            std.debug.print("not rendering {s}\n", .{@tagName(tag)});
        },
    }

    return false;
}

fn writeBlock(self: Self, writer: anytype, node: NodeIndex, depth: usize) @TypeOf(writer).Error!void {
    const lhs = self.tree.nodeLHS(node);

    try writer.writeAll("{");

    if (lhs != .none) {
        const statements = self.tree.spanToList(lhs);
        if (statements.len > 0) try writer.writeAll("\n");
        for (statements) |n| {
            const returned = try self.writeStatement(writer, n, depth);

            switch (self.tree.nodeTag(n)) {
                .comment,
                .@"if",
                .if_else,
                .if_else_if,
                .@"for",
                .@"while",
                .block,
                .@"switch",
                .loop,
                => {},
                else => try writer.writeAll(";"),
            }
            try writer.writeAll("\n");

            if (returned) break;
        }
    }

    for (0..depth - 1) |_| try writer.writeAll(self.tab);
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
