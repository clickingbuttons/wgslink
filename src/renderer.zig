const std = @import("std");
const Ast = @import("./wgsl/Ast.zig");
const Node = @import("./wgsl/Node.zig");
const Token = @import("./wgsl/Token.zig");

const NodeIndex = Node.Index;
const TokenIndex = Token.Index;
const Self = @This();

tree: *const Ast,
tab: []const u8,

pub fn writeTranslationUnit(tree: *const Ast, tab: []const u8, writer: anytype) !void {
    const r = Self{
        .tree = tree,
        .tab = tab,
    };

    for (r.tree.spanToList(0)) |node| {
        const tag = r.tree.nodes.items(.tag)[node];
        try switch (tag) {
            .diagnostic => r.writeDiagnostic(writer, node),
            .enable => r.writeEnable(writer, node),
            .requires => r.writeEnable(writer, node),
            .global_var => r.writeGlobalVar(writer, node),
            .override => r.writeOverride(writer, node),
            .@"const" => r.writeConst(writer, node),
            .@"struct" => r.writeStruct(writer, node),
            .@"fn" => r.writeFn(writer, node),
            .type_alias => r.writeTypeAlias(writer, node),
            .comment => r.writeComment(writer, node),
            .import => r.writeImport(writer, node),
            else => |t| {
                std.debug.print("could not render node {s}\n", .{@tagName(t)});
                unreachable;
            },
        };
        switch (tag) {
            .global_var, .override, .@"const", .type_alias => try writer.writeByte(';'),
            else => {},
        }

        try writer.writeAll("\n");
    }
}

fn writeNodeName(self: Self, writer: anytype, node: NodeIndex) !void {
    try writer.writeAll(self.tree.declNameSource(node));
}

fn writeNode(self: Self, writer: anytype, node: NodeIndex) !void {
    try writer.writeAll(self.tree.nodeSource(node));
}

fn writeToken(self: Self, writer: anytype, tok: TokenIndex) !void {
    try writer.writeAll(self.tree.tokenSource(tok));
}

fn writeEnable(self: Self, writer: anytype, node: NodeIndex) !void {
    try self.writeNode(writer, node);
    try writer.writeByte(' ');
    const enables = self.tree.spanToList(self.tree.nodeLHS(node));
    for (enables, 0..) |enable, i| {
        try writer.writeAll(self.tree.tokenSource(enable));
        if (i != enables.len - 1) try writer.writeAll(", ");
    }
    try writer.writeByte(';');
}

fn writeDiagnostic(self: Self, writer: anytype, node: NodeIndex) !void {
    try self.writeNode(writer, node);
    try writer.writeByte('(');
    try self.writeToken(writer, self.tree.nodeLHS(node));
    try writer.writeAll(", ");
    const rule = self.tree.extraData(Node.DiagnosticRule, self.tree.nodeRHS(node));
    try self.writeToken(writer, rule.name);
    if (rule.field != 0) {
        try writer.writeByte('.');
        try self.writeToken(writer, rule.field);
    }

    try writer.writeAll(");");
}

fn writeVar(self: Self, writer: anytype, extra: anytype, node: NodeIndex) !void {
    try writer.writeAll("var");

    if (extra.addr_space != 0) {
        try writer.writeByte('<');
        try writer.writeAll(self.tree.tokenSource(extra.addr_space));
    }

    if (extra.access_mode != 0) {
        try writer.writeByte(',');
        try writer.writeAll(self.tree.tokenSource(extra.access_mode));
    }

    if (extra.addr_space != 0) try writer.writeByte('>');

    try writer.writeByte(' ');
    try self.writeNodeName(writer, node);

    if (extra.type != 0) {
        try writer.writeAll(": ");
        try self.writeType(writer, extra.type);
    }

    const rhs = self.tree.nodeRHS(node);
    if (rhs != 0) {
        try writer.writeAll(" = ");
        try self.writeExpr(writer, rhs);
    }
}

fn writeGlobalVar(self: Self, writer: anytype, node: NodeIndex) !void {
    const extra = self.tree.extraData(Node.GlobalVar, self.tree.nodeLHS(node));

    if (extra.attrs != 0) {
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
            if (lhs != 0) {
                try writer.writeByte('<');
                try self.writeType(writer, lhs);
            }
            if (rhs != 0) {
                try writer.writeByte(',');
                try self.writeType(writer, rhs);
            }
            if (lhs != 0) try writer.writeByte('>');
        },
        .storage_texture_type => {
            try writer.writeByte('<');
            const tok1 = self.tree.nodeLHS(node);
            try writer.writeAll(self.tree.tokenSource(tok1));
            try writer.writeByte(',');
            const tok2 = self.tree.nodeRHS(node);
            try writer.writeAll(self.tree.tokenSource(tok2));
            try writer.writeByte('>');
        },
        .ptr_type => {
            const extra = self.tree.extraData(Node.PtrType, self.tree.nodeRHS(node));
            try writer.writeByte('<');
            try writer.writeAll(self.tree.tokenSource(extra.addr_space));
            try writer.writeByte(',');
            try self.writeType(writer, self.tree.nodeLHS(node));

            if (extra.access_mode != 0) {
                try writer.writeByte(',');
                try writer.writeAll(self.tree.tokenSource(extra.access_mode));
            }
            try writer.writeByte('>');
        },
        else => {},
    }
}

fn writeConst(self: Self, writer: anytype, node: NodeIndex) !void {
    const lhs = self.tree.nodeLHS(node);
    const rhs = self.tree.nodeRHS(node);
    try self.writeNode(writer, node);
    try writer.writeByte(' ');
    try self.writeNodeName(writer, node);
    if (lhs != 0) {
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
            try writer.writeByte('&');
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
            try writer.writeByte(' ');
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
            try writer.writeByte(' ');
            try self.writeExpr(writer, rhs);
        },
        .index_access => {
            try self.writeExpr(writer, lhs);
            try writer.writeByte('[');
            try self.writeExpr(writer, rhs);
            try writer.writeByte(']');
        },
        .field_access => {
            try self.writeExpr(writer, lhs);
            try writer.writeByte('.');
            try self.writeToken(writer, rhs);
        },
        .call => try self.writeCall(writer, node),
        .paren_expr => {
            try writer.writeByte('(');
            try self.writeExpr(writer, lhs);
            try writer.writeByte(')');
        },
        .bitcast => {
            try self.writeNode(writer, lhs);
            try writer.writeByte('(');
            try self.writeExpr(writer, rhs);
            try writer.writeByte(')');
        },
        else => |t| {
            std.debug.print("invalid expression {s}\n", .{@tagName(t)});
        },
    }
}

fn writeAttributes(self: Self, writer: anytype, attrs: NodeIndex) !void {
    for (self.tree.spanToList(attrs)) |attr| {
        try writer.writeByte('@');
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
        if (lhs != 0) {
            try writer.writeByte('(');
            switch (tag) {
                .attr_builtin, .attr_interpolate => try self.writeToken(writer, lhs),
                .attr_workgroup_size => {
                    const extra = self.tree.extraData(Node.WorkgroupSize, lhs);
                    try self.writeExpr(writer, extra.x);
                    if (extra.y != 0) {
                        try writer.writeByte(',');
                        try self.writeExpr(writer, extra.y);
                    }
                    if (extra.z != 0) {
                        try writer.writeByte(',');
                        try self.writeExpr(writer, extra.z);
                    }
                },
                else => try self.writeNode(writer, lhs),
            }
            try writer.writeByte(')');
        }
        try writer.writeByte(' ');
    }
}

fn writeStruct(self: Self, writer: anytype, node: NodeIndex) !void {
    try writer.writeAll("struct ");
    try self.writeNodeName(writer, node);
    try writer.writeAll(" {");
    const members = self.tree.spanToList(self.tree.nodeLHS(node));
    for (members) |m| {
        const name = self.tree.tokenSource(self.tree.nodeToken(m));
        try writer.writeAll("\n");
        try writer.writeAll(self.tab);
        const member_attrs_node = self.tree.nodeLHS(m);
        if (member_attrs_node != 0) try self.writeAttributes(writer, member_attrs_node);
        try writer.print("{s}: ", .{name});
        try self.writeType(writer, self.tree.nodeRHS(m));
        try writer.writeByte(',');
    }
    try writer.writeAll("\n}");
}

fn writeCall(self: Self, writer: anytype, node: NodeIndex) @TypeOf(writer).Error!void {
    const ty = self.tree.nodeRHS(node);
    if (ty != 0) try self.writeType(writer, ty) else try self.writeNode(writer, node);

    try writer.writeByte('(');
    const args = self.tree.nodeLHS(node);
    if (args != 0) {
        const list = self.tree.spanToList(args);
        for (list, 0..) |arg, i| {
            try self.writeExpr(writer, arg);
            if (i != list.len - 1) try writer.writeAll(", ");
        }
    }
    try writer.writeByte(')');
}

fn writeOverride(self: Self, writer: anytype, node: NodeIndex) !void {
    const extra = self.tree.extraData(Node.Override, self.tree.nodeLHS(node));

    if (extra.attrs != 0) try self.writeAttributes(writer, extra.attrs);

    try writer.writeAll("override ");
    try self.writeNodeName(writer, node);

    if (extra.type != 0) {
        try writer.writeAll(": ");
        try self.writeType(writer, extra.type);
    }

    const rhs = self.tree.nodeRHS(node);
    if (rhs != 0) {
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
    try writer.writeByte(' ');
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
            if (lhs != 0) {
                try writer.writeByte(' ');
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
            if (extra.init != 0) _ = try self.writeStatement(writer, extra.init, 0);
            try writer.writeAll("; ");
            if (extra.cond != 0) _ = try self.writeExpr(writer, extra.cond);
            try writer.writeAll("; ");
            if (extra.update != 0) _ = try self.writeStatement(writer, extra.update, 0);
            try writer.writeAll(") ");
            try self.writeBlock(writer, rhs, depth + 1);
        },
        .@"switch" => {
            try writer.writeAll("switch ");
            try self.writeExpr(writer, lhs);
            try writer.writeAll(" {");
            if (rhs != 0) for (self.tree.spanToList(rhs)) |c| {
                try writer.writeAll("\n");
                for (0..depth + 1) |_| try writer.writeAll(self.tab);
                switch (self.tree.nodeTag(c)) {
                    .switch_default, .switch_case_default => try writer.writeAll("default"),
                    .switch_case => {
                        const expr_list = self.tree.nodeLHS(c);
                        if (expr_list != 0) {
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
                try writer.writeByte(' ');
                try self.writeBlock(writer, self.tree.nodeRHS(c), depth + 2);
            };
            try writer.writeAll("\n");
            for (0..depth) |_| try writer.writeAll(self.tab);
            try writer.writeByte('}');
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

    try writer.writeByte('{');

    if (lhs != 0) {
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
                else => try writer.writeByte(';'),
            }
            try writer.writeAll("\n");

            if (returned) break;
        }
    }

    for (0..depth - 1) |_| try writer.writeAll(self.tab);
    try writer.writeByte('}');
}

fn writeFn(self: Self, writer: anytype, node: NodeIndex) !void {
    const extra = self.tree.extraData(Node.FnProto, self.tree.nodeLHS(node));

    if (extra.attrs != 0) try self.writeAttributes(writer, extra.attrs);

    try writer.writeAll("fn ");
    try self.writeNodeName(writer, node);
    try writer.writeByte('(');

    if (extra.params != 0) {
        const params = self.tree.spanToList(extra.params);
        for (params, 0..) |p, i| {
            const attrs = self.tree.nodeLHS(p);
            if (attrs != 0) try self.writeAttributes(writer, attrs);

            try self.writeNode(writer, p);
            try writer.writeAll(": ");
            try self.writeType(writer, self.tree.nodeRHS(p));
            if (i != params.len - 1) try writer.writeByte(',');
        }
    }
    try writer.writeByte(')');
    if (extra.return_type != 0) {
        try writer.writeAll(" -> ");
        if (extra.return_attrs != 0) try self.writeAttributes(writer, extra.return_attrs);
        try self.writeType(writer, extra.return_type);
    }
    try writer.writeByte(' ');
    try self.writeBlock(writer, self.tree.nodeRHS(node), 1);
}

fn writeComment(self: Self, writer: anytype, node: NodeIndex) !void {
    try self.writeNode(writer, node);
}

fn writeImport(self: Self, writer: anytype, node: NodeIndex) !void {
    const imports = self.tree.nodeLHS(node);
    const mod_name = self.tree.tokenSource(self.tree.nodeRHS(node));

    try writer.writeAll("// import ");
    if (imports != 0) {
        try writer.writeAll("{ ");
        const list = self.tree.spanToList(imports);
        for (list, 0..) |n, i| {
            try self.writeNode(writer, n);
            if (i != list.len - 1) try writer.writeAll(", ");
        }
        try writer.writeAll(" } ");
    }
    try writer.print("from {s};", .{mod_name});
}

fn writeFormatted(
    allocator: std.mem.Allocator,
    writer: anytype,
    source: [:0]const u8,
) !void {
    var ast = try Ast.init(allocator, source);
    defer ast.deinit(allocator);
    const stderr = std.io.getStdErr();
    const term = std.io.tty.detectConfig(stderr);
    for (ast.errors) |e| try ast.renderError(e, stderr.writer(), term);
    if (ast.errors.len == 0) try writeTranslationUnit(&ast, "  ", writer);
}

fn testRender(comptime source: [:0]const u8, comptime expected: [:0]const u8) !void {
    const allocator = std.testing.allocator;
    var arr = try std.ArrayList(u8).initCapacity(allocator, source.len);
    defer arr.deinit();

    try writeFormatted(allocator, arr.writer(), source);

    try std.testing.expectEqualStrings(expected ++ "\n", arr.items);
}

fn testCanonical(comptime source: [:0]const u8) !void {
    try testRender(source, source);
}

test "array type" {
    try testCanonical("var a: array<u32,3> = array<u32,3>(0u, 1u, 2u);");
}

test "enable" {
    try testCanonical("enable f16;");
    try testCanonical("enable f16, f16;");
}

test "diagnostic" {
    try testCanonical("diagnostic(warning, foo);");
    try testCanonical("diagnostic(error, foo.bar);");
}

test "requires" {
    try testCanonical("requires readonly_and_readwrite_storage_textures;");
    try testCanonical("requires foo, bar;");
}

test "const" {
    try testCanonical("const a: u32 = 0u;");
    try testCanonical("const b: Foo = Foo();");
    try testCanonical("const c: vec2f = vec2f();");
    try testCanonical("const d: vec2f = vec2f(1.0);");
}

test "struct" {
    const foo =
        \\struct Foo {
        \\  bar: u32,
        \\  baz: i32,
        \\}
    ;
    try testCanonical(foo);

    try testRender(foo ++ ";", foo);

    try testCanonical(
        \\struct Foo {
        \\  @align(16) @size(4) bar: u32,
        \\  baz: i32,
        \\}
    );

    try testCanonical(
        \\struct VertexInput {
        \\  @builtin(vertex_index) vertex: u32,
        \\  @builtin(instance_index) instance: u32,
        \\}
        \\struct VertexOutput {
        \\  @builtin(position) position: vec4f,
        \\  @location(0) color: vec4f,
        \\  @location(1) worldPos: vec4f,
        \\  @location(2) normal: vec3f,
        \\}
    );
}

test "global var" {
    try testCanonical("@group(g_scene) @binding(0) var<uniform> view: View;");
}

test "var types" {
    try testCanonical("var a: vec2<Test>;");
    try testCanonical("var b: array<i32>;");
    try testCanonical("var b: mat4x4<f32>;");
    try testCanonical("var b: mat4x4f;");

    try testCanonical("var ptr_int: ptr<function,i32>;");
    try testCanonical("var ptr_int2: ptr<function,i32,read>;");

    try testCanonical("var sam: sampler;");
    try testCanonical("var tex: texture_2d<f32>;");

    try testCanonical("var tex2: texture_multisampled_2d<i32>;");
    try testCanonical("var tex3: texture_depth_multisampled_2d;");

    try testCanonical("var tex4: texture_storage_2d<rgba8unorm,read>;");

    try testCanonical("var tex5: texture_depth_2d;");
    try testCanonical("var tex6: texture_external;");
}

test "override" {
    try testCanonical("@id(0) override wireframe: bool = false;");
}

test "type alias" {
    try testCanonical("alias single = f32;");
}

test "field access" {
    try testCanonical("var a = b.c;");
}

test "comments" {
    try testRender(
        \\// Hello there
        \\const a: u32 = 0u;
        \\// Goodbye there
        \\const b: u32 = 0u;
        \\/* Block here */
        \\const c: u32 = 0u;
    ,
        \\const a: u32 = 0u;
        \\const b: u32 = 0u;
        \\const c: u32 = 0u;
    );
}

test "function" {
    try testCanonical(
        \\fn view32() -> @location(0) mat4x4f {
        \\  var res = mat4x4f(view.view);
        \\  res[3][0] = 0.0;
        \\  res[3][1] = 0.0;
        \\  res[3][2] = 0.0;
        \\  return res;
        \\}
    );
}

test "entry function" {
    try testCanonical("@vertex fn main() {}");
    try testCanonical("@compute @workgroup_size(1,2,3) fn main() {}");
}

test "paren" {
    try testCanonical("const a = (3 - 2);");
    try testCanonical("const a = ((3 - 2));");
}

test "switch" {
    try testCanonical(
        \\fn test() {
        \\  switch a {
        \\    case 3 {
        \\      b = 2;
        \\      break;
        \\    }
        \\    default {
        \\      break;
        \\    }
        \\  }
        \\}
    );
}

test "loop" {
    try testCanonical(
        \\fn test() {
        \\  loop {
        \\    if (i >= 4) {
        \\      break;
        \\    }
        \\    i++;
        \\  }
        \\}
    );
}

test "naked if" {
    try testCanonical(
        \\fn test() {
        \\  if false {
        \\    return;
        \\  }
        \\}
    );
}

// test "prune" {
//     try testPretty(
//         \\fn pruneMe() -> u32 {
//         \\  return 0u;
//         \\}
//         \\@vertex fn main() {}
//     ,
//         \\@vertex fn main() {}
//     , true);
// }

test "import" {
    try testCanonical("// import { Foo } from './foo.wgsl';");
}
