const std = @import("std");
const Ast = @import("./wgsl/Ast.zig");
const Node = @import("./wgsl/Node.zig");
const Token = @import("./wgsl/Token.zig");

/// Automatically inserts indentation of written data by keeping
/// track of the current indentation level
pub fn Renderer(comptime UnderlyingWriter: type) type {
    return struct {
        const Self = @This();
        const NodeIndex = Node.Index;
        const TokenIndex = Token.Index;
        pub const WriteError = UnderlyingWriter.Error;
        pub const Writer = std.io.Writer(*Self, WriteError, write);

        underlying_writer: UnderlyingWriter,

        /// Offset into the source at which formatting has been disabled with
        /// a `zig fmt: off` comment.
        ///
        /// If non-null, the AutoIndentingStream will not write any bytes
        /// to the underlying self. It will however continue to track the
        /// indentation level.
        disabled_offset: ?usize = null,

        indent_count: usize = 0,
        indent_delta: usize = 2,
        current_line_empty: bool = true,
        /// automatically popped when applied
        indent_one_shot_count: usize = 0,
        /// the most recently applied indent
        applied_indent: usize = 0,
        /// not used until the next line
        indent_next_line: usize = 0,

        tree: *const Ast,

        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }

        pub fn write(self: *Self, bytes: []const u8) WriteError!usize {
            if (bytes.len == 0)
                return @as(usize, 0);

            try self.applyIndent();
            return self.writeNoIndent(bytes);
        }

        pub fn writeAll(self: *Self, bytes: []const u8) WriteError!void {
            return self.writer().writeAll(bytes);
        }

        pub fn writeByte(self: *Self, byte: u8) WriteError!void {
            return self.writer().writeByte(byte);
        }

        pub fn print(self: *Self, comptime format: []const u8, args: anytype) WriteError!void {
            return self.writer().print(format, args);
        }

        // Change the indent delta without changing the final indentation level
        pub fn setIndentDelta(self: *Self, new_indent_delta: usize) void {
            if (self.indent_delta == new_indent_delta) {
                return;
            } else if (self.indent_delta > new_indent_delta) {
                std.debug.assert(self.indent_delta % new_indent_delta == 0);
                self.indent_count = self.indent_count * (self.indent_delta / new_indent_delta);
            } else {
                // assert that the current indentation (in spaces) in a multiple of the new delta
                std.debug.assert((self.indent_count * self.indent_delta) % new_indent_delta == 0);
                self.indent_count = self.indent_count / (new_indent_delta / self.indent_delta);
            }
            self.indent_delta = new_indent_delta;
        }

        fn writeNoIndent(self: *Self, bytes: []const u8) WriteError!usize {
            if (bytes.len == 0)
                return @as(usize, 0);

            if (self.disabled_offset == null) try self.underlying_writer.writeAll(bytes);
            if (bytes[bytes.len - 1] == '\n')
                self.resetLine();
            return bytes.len;
        }

        pub fn insertNewline(self: *Self) WriteError!void {
            _ = try self.writeNoIndent("\n");
        }

        fn resetLine(self: *Self) void {
            self.current_line_empty = true;
            self.indent_next_line = 0;
        }

        /// Insert a newline unless the current line is blank
        pub fn maybeInsertNewline(self: *Self) WriteError!void {
            if (!self.current_line_empty)
                try self.insertNewline();
        }

        /// Push default indentation
        /// Doesn't actually write any indentation.
        /// Just primes the stream to be able to write the correct indentation if it needs to.
        pub fn pushIndent(self: *Self) void {
            self.indent_count += 1;
        }

        /// Push an indent that is automatically popped after being applied
        pub fn pushIndentOneShot(self: *Self) void {
            self.indent_one_shot_count += 1;
            self.pushIndent();
        }

        /// Turns all one-shot indents into regular indents
        /// Returns number of indents that must now be manually popped
        pub fn lockOneShotIndent(self: *Self) usize {
            var locked_count = self.indent_one_shot_count;
            self.indent_one_shot_count = 0;
            return locked_count;
        }

        /// Push an indent that should not take effect until the next line
        pub fn pushIndentNextLine(self: *Self) void {
            self.indent_next_line += 1;
            self.pushIndent();
        }

        pub fn popIndent(self: *Self) void {
            std.debug.assert(self.indent_count != 0);
            self.indent_count -= 1;

            if (self.indent_next_line > 0)
                self.indent_next_line -= 1;
        }

        /// Writes ' ' bytes if the current line is empty
        fn applyIndent(self: *Self) WriteError!void {
            const current_indent = self.currentIndent();
            if (self.current_line_empty and current_indent > 0) {
                if (self.disabled_offset == null) {
                    try self.underlying_writer.writeByteNTimes(' ', current_indent);
                }
                self.applied_indent = current_indent;
            }

            self.indent_count -= self.indent_one_shot_count;
            self.indent_one_shot_count = 0;
            self.current_line_empty = false;
        }

        /// Checks to see if the most recent indentation exceeds the currently pushed indents
        pub fn isLineOverIndented(self: *Self) bool {
            if (self.current_line_empty) return false;
            return self.applied_indent > self.currentIndent();
        }

        fn currentIndent(self: *Self) usize {
            var indent_current: usize = 0;
            if (self.indent_count > 0) {
                const indent_count = self.indent_count - self.indent_next_line;
                indent_current = indent_count * self.indent_delta;
            }
            return indent_current;
        }

        pub fn writeTranslationUnit(self: *Self) !void {
            for (self.tree.spanToList(0)) |node| {
                const tag = self.tree.nodeTag(node);
                try switch (tag) {
                    .diagnostic_directive => self.writeGlobalDiagnostic(node),
                    .enable_directive => self.writeEnable(node),
                    .requires_directive => self.writeEnable(node),
                    .global_var => self.writeGlobalVar(node),
                    .override => self.writeOverride(node),
                    .@"const" => self.writeConst(node),
                    .@"struct" => self.writeStruct(node),
                    .@"fn" => self.writeFn(node),
                    .type_alias => self.writeTypeAlias(node),
                    .comment => self.writeComment(node),
                    .import => self.writeImport(node),
                    else => |t| {
                        std.debug.print("could not render node {s}\n", .{@tagName(t)});
                        unreachable;
                    },
                };
                switch (tag) {
                    .global_var,
                    .override,
                    .@"const",
                    .type_alias,
                    .diagnostic_directive,
                    .enable_directive,
                    .requires_directive,
                    => try self.writeByte(';'),
                    else => {},
                }

                try self.writeAll("\n");
            }
        }

        fn writeNodeName(self: *Self, node: NodeIndex) !void {
            try self.writeAll(self.tree.declNameSource(node));
        }

        fn writeNode(self: *Self, node: NodeIndex) !void {
            try self.writeAll(self.tree.nodeSource(node));
        }

        fn writeToken(self: *Self, tok: TokenIndex) !void {
            try self.writeAll(self.tree.tokenSource(tok));
        }

        fn writeEnable(self: *Self, node: NodeIndex) !void {
            try self.writeNode(node);
            try self.writeByte(' ');
            const enables = self.tree.spanToList(self.tree.nodeLHS(node));
            for (enables, 0..) |enable, i| {
                try self.writeAll(self.tree.tokenSource(enable));
                if (i != enables.len - 1) try self.writeAll(", ");
            }
        }

        fn writeDiagnosticRule(self: *Self, node: NodeIndex) !void {
            try self.writeToken(self.tree.nodeLHS(node));
            try self.writeAll(", ");
            const rule = self.tree.extraData(Node.DiagnosticRule, self.tree.nodeRHS(node));
            try self.writeToken(rule.name);
            if (rule.field != 0) {
                try self.writeByte('.');
                try self.writeToken(rule.field);
            }
        }

        fn writeGlobalDiagnostic(self: *Self, node: NodeIndex) !void {
            try self.writeNode(node);
            try self.writeByte('(');
            try self.writeDiagnosticRule(node);
            try self.writeByte(')');
        }

        fn writeVar(self: *Self, extra: anytype, node: NodeIndex) !void {
            try self.writeAll("var");

            if (extra.addr_space != 0) {
                try self.writeByte('<');
                try self.writeAll(self.tree.tokenSource(extra.addr_space));
            }

            if (extra.access_mode != 0) {
                try self.writeByte(',');
                try self.writeAll(self.tree.tokenSource(extra.access_mode));
            }

            if (extra.addr_space != 0) try self.writeByte('>');

            try self.writeByte(' ');
            try self.writeNodeName(node);

            if (extra.type != 0) {
                try self.writeAll(": ");
                try self.writeType(extra.type);
            }

            const rhs = self.tree.nodeRHS(node);
            if (rhs != 0) {
                try self.writeAll(" = ");
                try self.writeExpr(rhs);
            }
        }

        fn writeGlobalVar(self: *Self, node: NodeIndex) !void {
            const extra = self.tree.extraData(Node.GlobalVar, self.tree.nodeLHS(node));

            if (extra.attrs != 0) {
                for (self.tree.spanToList(extra.attrs)) |attr| {
                    switch (self.tree.nodeTag(attr)) {
                        .attr_group => {
                            try self.writeAll("@group(");
                            try self.writeExpr(self.tree.nodeLHS(attr));
                            try self.writeAll(") ");
                        },
                        .attr_binding => {
                            try self.writeAll("@binding(");
                            try self.writeExpr(self.tree.nodeLHS(attr));
                            try self.writeAll(") ");
                        },
                        else => {},
                    }
                }
            }

            try self.writeVar(extra, node);
        }

        fn writeTemplateElaboratedIdent(self: *Self, node: NodeIndex) !void {
            try self.writeNode(node);
            const lhs = self.tree.nodeLHS(node);
            if (lhs != 0) {
                try self.writeByte('<');
                const args = self.tree.spanToList(lhs);
                for (args, 0..) |n, i| {
                    try self.writeExpr(n);
                    if (i != args.len - 1) try self.writeByte(',');
                }
                try self.writeByte('>');
            }
        }

        fn writeType(self: *Self, node: NodeIndex) !void {
            try self.writeTemplateElaboratedIdent(node);
        }

        fn writeConst(self: *Self, node: NodeIndex) !void {
            const lhs = self.tree.nodeLHS(node);
            const rhs = self.tree.nodeRHS(node);
            try self.writeNode(node);
            try self.writeByte(' ');
            try self.writeNodeName(node);
            if (lhs != 0) {
                try self.writeAll(": ");
                try self.writeType(lhs);
            }
            try self.writeAll(" = ");
            try self.writeExpr(rhs);
        }

        fn writeExpr(self: *Self, node: NodeIndex) !void {
            const tag = self.tree.nodeTag(node);
            const lhs = self.tree.nodeLHS(node);
            const rhs = self.tree.nodeRHS(node);
            switch (tag) {
                .number, .ident, .true, .false => try self.writeNode(node),
                .not, .negate, .deref => {
                    try self.writeNode(node);
                    try self.writeExpr(lhs);
                },
                .addr_of => {
                    try self.writeByte('&');
                    try self.writeExpr(lhs);
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
                    try self.writeExpr(lhs);
                    try self.writeByte(' ');
                    try self.writeAll(switch (t) {
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
                    try self.writeByte(' ');
                    try self.writeExpr(rhs);
                },
                .index_access => {
                    try self.writeExpr(lhs);
                    try self.writeByte('[');
                    try self.writeExpr(rhs);
                    try self.writeByte(']');
                },
                .field_access => {
                    try self.writeExpr(lhs);
                    try self.writeByte('.');
                    try self.writeToken(rhs);
                },
                .call => try self.writeCall(node),
                .paren_expr => {
                    try self.writeByte('(');
                    try self.writeExpr(lhs);
                    try self.writeByte(')');
                },
                else => |t| {
                    std.debug.print("invalid expression {s}\n", .{@tagName(t)});
                },
            }
        }

        fn writeAttributes(self: *Self, attrs: NodeIndex) !void {
            for (self.tree.spanToList(attrs)) |attr| {
                try self.writeByte('@');
                const tag = self.tree.nodeTag(attr);
                const attr_name = @tagName(tag)["attr_".len..];
                try self.writeAll(attr_name);
                const lhs = self.tree.nodeLHS(attr);
                if (lhs != 0) {
                    try self.writeByte('(');
                    switch (tag) {
                        .attr_builtin, .attr_interpolate => try self.writeToken(lhs),
                        .attr_diagnostic => try self.writeDiagnosticRule(attr),
                        .attr_workgroup_size => {
                            const extra = self.tree.extraData(Node.WorkgroupSize, lhs);
                            try self.writeExpr(extra.x);
                            if (extra.y != 0) {
                                try self.writeByte(',');
                                try self.writeExpr(extra.y);
                            }
                            if (extra.z != 0) {
                                try self.writeByte(',');
                                try self.writeExpr(extra.z);
                            }
                        },
                        else => try self.writeNode(lhs),
                    }
                    try self.writeByte(')');
                }
                try self.writeByte(' ');
            }
        }

        fn writeStruct(self: *Self, node: NodeIndex) !void {
            try self.writeAll("struct ");
            try self.writeNodeName(node);
            try self.writeAll(" {");
            self.pushIndentNextLine();
            const members = self.tree.spanToList(self.tree.nodeLHS(node));
            for (members) |m| {
                const name = self.tree.tokenSource(self.tree.nodeToken(m));
                try self.writeAll("\n");
                const member_attrs_node = self.tree.nodeLHS(m);
                if (member_attrs_node != 0) try self.writeAttributes(member_attrs_node);
                try self.print("{s}: ", .{name});
                try self.writeType(self.tree.nodeRHS(m));
                try self.writeByte(',');
            }
            self.popIndent();
            try self.writeAll("\n}");
        }

        fn writeCall(self: *Self, node: NodeIndex) WriteError!void {
            try self.writeTemplateElaboratedIdent(self.tree.nodeLHS(node));
            try self.writeByte('(');
            const args = self.tree.nodeRHS(node);
            if (args != 0) {
                const list = self.tree.spanToList(args);
                for (list, 0..) |arg, i| {
                    try self.writeExpr(arg);
                    if (i != list.len - 1) try self.writeAll(", ");
                }
            }
            try self.writeByte(')');
        }

        fn writeOverride(self: *Self, node: NodeIndex) !void {
            const extra = self.tree.extraData(Node.Override, self.tree.nodeLHS(node));

            if (extra.attrs != 0) try self.writeAttributes(extra.attrs);

            try self.writeAll("override ");
            try self.writeNodeName(node);

            if (extra.type != 0) {
                try self.writeAll(": ");
                try self.writeType(extra.type);
            }

            const rhs = self.tree.nodeRHS(node);
            if (rhs != 0) {
                try self.writeAll(" = ");
                try self.writeExpr(rhs);
            }
        }

        fn writeTypeAlias(self: *Self, node: NodeIndex) !void {
            try self.writeAll("alias ");
            try self.writeNodeName(node);
            try self.writeAll(" = ");
            try self.writeNode(self.tree.nodeLHS(node));
        }

        fn writeIf(self: *Self, node: NodeIndex) !void {
            try self.writeAll("if ");
            try self.writeExpr(self.tree.nodeLHS(node));
            try self.writeByte(' ');
            try self.writeCompoundStatement(self.tree.nodeRHS(node));
        }

        fn writeStatement(self: *Self, node: NodeIndex) WriteError!bool {
            const tag = self.tree.nodeTag(node);
            const lhs = self.tree.nodeLHS(node);
            const rhs = self.tree.nodeRHS(node);

            switch (tag) {
                .compound_assign => {
                    try self.writeExpr(lhs);
                    try self.writeAll(" = ");
                    try self.writeExpr(rhs);
                },
                .phony_assign => {
                    try self.writeAll("_ = ");
                    try self.writeExpr(lhs);
                },
                .call => try self.writeCall(node),
                .@"return" => {
                    try self.writeAll("return");
                    if (lhs != 0) {
                        try self.writeByte(' ');
                        try self.writeExpr(lhs);
                    }
                    return true;
                },
                .comment => try self.writeComment(node),
                .break_if => {
                    try self.writeAll("break if ");
                    try self.writeExpr(lhs);
                },
                .@"if" => try self.writeIf(node),
                .else_if => {
                    try self.writeIf(lhs);
                    try self.writeAll(" else ");
                    _ = try self.writeStatement(rhs);
                },
                .@"else" => {
                    try self.writeIf(lhs);
                    try self.writeAll(" else ");
                    try self.writeCompoundStatement(rhs);
                },
                .@"while" => {
                    try self.writeAll("while ");
                    try self.writeExpr(lhs);
                    try self.writeAll(" ");
                    try self.writeCompoundStatement(rhs);
                },
                .@"for" => {
                    const extra = self.tree.extraData(Node.ForHeader, self.tree.nodeLHS(node));
                    if (extra.attrs != 0) try self.writeAttributes(extra.attrs);
                    try self.writeAll("for (");
                    if (extra.init != 0) _ = try self.writeStatement(extra.init);
                    try self.writeAll("; ");
                    if (extra.cond != 0) _ = try self.writeExpr(extra.cond);
                    try self.writeAll("; ");
                    if (extra.update != 0) _ = try self.writeStatement(extra.update);
                    try self.writeAll(") ");
                    try self.writeCompoundStatement(rhs);
                },
                .@"switch" => {
                    try self.writeAll("switch ");
                    try self.writeExpr(lhs);
                    try self.writeAll(" {");
                    if (rhs != 0) for (self.tree.spanToList(rhs)) |c| {
                        try self.writeAll("\n");
                        switch (self.tree.nodeTag(c)) {
                            .switch_default, .switch_case_default => try self.writeAll("default"),
                            .switch_case => {
                                const expr_list = self.tree.nodeLHS(c);
                                if (expr_list != 0) {
                                    try self.writeAll("case ");
                                    const expressions = self.tree.spanToList(expr_list);
                                    for (expressions, 0..) |e, i| {
                                        try self.writeExpr(e);
                                        if (i != expressions.len - 1) try self.writeAll(", ");
                                    }
                                }
                            },
                            else => unreachable,
                        }
                        try self.writeByte(' ');
                        try self.writeCompoundStatement(self.tree.nodeRHS(c));
                    };
                    try self.writeByte('\n');
                    try self.writeByte('}');
                },
                .loop => {
                    if (lhs != 0) try self.writeAttributes(lhs);
                    try self.writeAll("loop ");
                    try self.writeCompoundStatement(rhs);
                },
                .compound_statement => try self.writeCompoundStatement(node),
                .continuing => {
                    try self.writeAll("continuing ");
                    try self.writeCompoundStatement(lhs);
                },
                .discard => try self.writeAll("discard"),
                .@"break" => try self.writeAll("break"),
                .@"continue" => try self.writeAll("continue"),
                .increase, .decrease => {
                    try self.writeExpr(lhs);
                    try self.writeNode(node);
                },
                .@"const", .let => try self.writeConst(node),
                .@"var" => {
                    const extra = self.tree.extraData(Node.Var, self.tree.nodeLHS(node));
                    try self.writeVar(extra, node);
                },
                else => {
                    std.debug.print("not rendering {s}\n", .{@tagName(tag)});
                },
            }

            return false;
        }

        fn writeCompoundStatement(self: *Self, node: NodeIndex) WriteError!void {
            const lhs = self.tree.nodeLHS(node);
            const rhs = self.tree.nodeRHS(node);

            if (lhs != 0) try self.writeAttributes(lhs);
            try self.writeByte('{');
            self.pushIndentNextLine();

            if (rhs != 0) {
                const statements = self.tree.spanToList(rhs);
                if (statements.len > 0) try self.writeByte('\n');
                for (statements) |n| {
                    const returned = try self.writeStatement(n);

                    switch (self.tree.nodeTag(n)) {
                        .comment,
                        .@"if",
                        .else_if,
                        .@"else",
                        .@"for",
                        .@"while",
                        .compound_statement,
                        .@"switch",
                        .loop,
                        .continuing,
                        => {},
                        else => try self.writeByte(';'),
                    }
                    try self.writeAll("\n");

                    if (returned) break;
                }
            }

            self.popIndent();
            try self.writeByte('}');
        }

        fn writeFn(self: *Self, node: NodeIndex) !void {
            const extra = self.tree.extraData(Node.FnProto, self.tree.nodeLHS(node));

            if (extra.attrs != 0) try self.writeAttributes(extra.attrs);

            try self.writeAll("fn ");
            try self.writeNodeName(node);
            try self.writeByte('(');

            if (extra.params != 0) {
                const params = self.tree.spanToList(extra.params);
                for (params, 0..) |p, i| {
                    const attrs = self.tree.nodeLHS(p);
                    if (attrs != 0) try self.writeAttributes(attrs);

                    try self.writeNode(p);
                    try self.writeAll(": ");
                    try self.writeType(self.tree.nodeRHS(p));
                    if (i != params.len - 1) try self.writeByte(',');
                }
            }
            try self.writeByte(')');
            if (extra.return_type != 0) {
                try self.writeAll(" -> ");
                if (extra.return_attrs != 0) try self.writeAttributes(extra.return_attrs);
                try self.writeType(extra.return_type);
            }
            try self.writeByte(' ');
            try self.writeCompoundStatement(self.tree.nodeRHS(node));
        }

        fn writeComment(self: *Self, node: NodeIndex) !void {
            try self.writeNode(node);
        }

        fn writeImport(self: *Self, node: NodeIndex) !void {
            const imports = self.tree.nodeLHS(node);
            const mod_name = self.tree.tokenSource(self.tree.nodeRHS(node));

            try self.writeAll("// import ");
            if (imports != 0) {
                try self.writeAll("{ ");
                const list = self.tree.spanToList(imports);
                for (list, 0..) |n, i| {
                    try self.writeNode(n);
                    if (i != list.len - 1) try self.writeAll(", ");
                }
                try self.writeAll(" } ");
            }
            try self.print("from {s};", .{mod_name});
        }
    };
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
    if (ast.errors.len > 0) try stderr.writer().writeByte('\n');
    for (ast.errors) |e| try ast.renderError(e, stderr.writer(), term);
    if (ast.errors.len == 0) {
        var renderer = Renderer(@TypeOf(writer)){
            .tree = &ast,
            .underlying_writer = writer,
        };
        try renderer.writeTranslationUnit();
    }
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

    try testCanonical("@diagnostic(error, foo.bar) fn main() {}");
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
        \\  case 3 {
        \\    b = 2;
        \\    break;
        \\  }
        \\  default {
        \\    break;
        \\  }
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

    try testCanonical(
        \\fn test() {
        \\  loop {
        \\    if i % 2 == 0 {
        \\      continue;
        \\    }
        \\    continuing {
        \\      i++;
        \\      break if i >= 4;
        \\    }
        \\  }
        \\}
    );
}

test "if" {
    try testCanonical(
        \\fn test() {
        \\  if false {
        \\    return;
        \\  }
        \\}
    );

    try testCanonical(
        \\fn test() {
        \\  if false {
        \\    return;
        \\  } else if true {
        \\    return;
        \\  } else {
        \\    return;
        \\  }
        \\}
    );
}

test "while" {
    try testCanonical(
        \\fn test() {
        \\  while false {
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
