const std = @import("std");
const Ast = @import("./ast/Ast.zig");
const node_mod = @import("./ast/Node.zig");
const Parser = @import("./wgsl/Parser.zig");
const Visitor = @import("./ast/Visitor.zig").Visitor;
const Token = @import("./wgsl/Token.zig").Tag;

const Allocator = std.mem.Allocator;
const Node = node_mod.Node;

/// Renders bundles of modules back into WGSL.
pub fn Renderer(comptime UnderlyingWriter: type) type {
    return struct {
        const Self = @This();
        pub const WriteError = UnderlyingWriter.Error || error{OutOfMemory};
        pub const Writer = std.io.Writer(*Self, WriteError, write);
        const SpanOpts = struct {
            start: []const u8 = "",
            sep: []const u8 = "",
            sep_space: bool = false,
            end: []const u8 = "",
        };
        const SpanStack = std.ArrayList(SpanOpts);
        const RetType = WriteError!void;
        const VisitorType = Visitor(RetType);

        underlying_writer: UnderlyingWriter,
        minify: bool,
        imports: bool,

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

        pub fn init(underlying_writer: UnderlyingWriter, minify: bool, imports: bool) Self {
            return Self{
                .underlying_writer = underlying_writer,
                .minify = minify,
                .imports = imports,
            };
        }

        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }

        pub fn write(self: *Self, bytes: []const u8) WriteError!usize {
            if (bytes.len == 0)
                return @as(usize, 0);

            try self.applyIndent();
            return self.writeNoIndent(bytes);
        }

        pub fn writeAll(self: *Self, bytes: []const u8) RetType {
            return self.writer().writeAll(bytes);
        }

        pub fn writeByte(self: *Self, byte: u8) RetType {
            return self.writer().writeByte(byte);
        }

        pub fn print(self: *Self, comptime format: []const u8, args: anytype) RetType {
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
            if (bytes.len == 0) return 0;
            if (self.disabled_offset == null) try self.underlying_writer.writeAll(bytes);
            if (bytes[bytes.len - 1] == '\n') self.resetLine();
            return bytes.len;
        }

        pub fn newline(self: *Self) RetType {
            if (!self.minify) _ = try self.writeNoIndent("\n");
        }

        fn resetLine(self: *Self) void {
            self.current_line_empty = true;
            self.indent_next_line = 0;
        }

        /// Insert a newline unless the current line is blank
        pub fn maybeInsertNewline(self: *Self) RetType {
            if (!self.current_line_empty)
                try self.newline();
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
        fn applyIndent(self: *Self) RetType {
            if (self.minify) return;
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

        fn writeSpace(self: *Self) !void {
            if (!self.minify) try self.writeByte(' ');
        }

        fn writeSpaced(self: *Self, token: []const u8) !void {
            try self.writeSpace();
            try self.writeAll(token);
            try self.writeSpace();
        }

        pub fn writeTranslationUnit(self: *Self, tree: Ast) !void {
            const indices = tree.spanToList(0);
            for (indices, 0..) |index, i| {
                std.debug.assert(index != 0);
                // std.debug.print("{d} / {d} = {d} {any}\n", .{ i, indices.len, index, tree.node(index) });
                try self.writeIndex(tree, index);
                switch (tree.node(index)) {
                    .import => if (!self.imports) continue,
                    else => {},
                }
                switch (tree.node(index)) {
                    .diagnostic_directive,
                    .enable_directive,
                    .requires_directive,
                    .global_var,
                    .override,
                    .@"const",
                    .type_alias,
                    .const_assert,
                    .import,
                    => try self.writeToken(.@";"),
                    else => {},
                }
                if (i != indices.len - 1) try self.newline();
            }
        }

        fn writeNode(self: *Self, tree: Ast, node: Node) RetType {
            switch (node) {
                inline .@"error", .span => |_, tag| {
                    std.debug.panic("unexpected node of type {s}", .{@tagName(tag)});
                },
                .comment => |n| {
                    if (self.minify) return;
                    try self.writeAll("// ");
                    try self.writeIdentifier(tree, n.ident);
                },
                .diagnostic_directive => |n| {
                    try self.writeToken(.k_diagnostic);
                    try self.writeDiagnostic(tree, n.diagnostic_control);
                },
                .enable_directive => |n| {
                    try self.writeTokenSpace(.k_enable);
                    try self.writeIdentList(tree, n.idents, .{ .sep = "," });
                },
                .requires_directive => |n| {
                    try self.writeTokenSpace(.k_requires);
                    try self.writeIdentList(tree, n.idents, .{ .sep = "," });
                },
                .global_var => |n| {
                    const global_var = tree.extraData(node_mod.GlobalVar, n.global_var);
                    try self.writeAttributes(tree, global_var.attrs);
                    try self.writeToken(.k_var);
                    try self.writeTemplateList(tree, global_var.template_list);
                    if (global_var.template_list != 0) try self.writeSpace() else try self.writeByte(' ');
                    try self.writeOptionallyTypedIdent(tree, global_var.name, global_var.type, n.initializer);
                },
                .override => |n| {
                    const override = tree.extraData(node_mod.Override, n.override);
                    try self.writeAttributes(tree, override.attrs);
                    try self.writeTokenSpace(.k_override);
                    try self.writeOptionallyTypedIdent(tree, override.name, override.type, n.initializer);
                },
                .@"fn" => |n| {
                    const header = tree.extraData(node_mod.FnHeader, n.fn_header);
                    try self.writeAttributes(tree, header.attrs);
                    try self.writeTokenSpace(.k_fn);
                    try self.writeIdentifier(tree, header.name);
                    try self.writeToken(.@"(");
                    try self.writeList(tree, header.params, .{ .sep_space = true });
                    try self.writeToken(.@")");
                    if (header.return_type != 0) {
                        try self.writeTokenSpaced(.@"->");
                        try self.writeAttributes(tree, header.return_attrs);
                        try self.writeIndex(tree, header.return_type);
                    }
                    try self.writeSpace();
                    try self.writeIndex(tree, n.body);
                },
                .@"const", .let => |n| {
                    const typed_ident = tree.extraData(node_mod.TypedIdent, n.typed_ident);
                    const token = switch (node) {
                        .@"const" => Token.k_const,
                        .let => Token.k_let,
                        else => unreachable,
                    };
                    try self.writeAll(token.symbol());
                    try self.writeByte(' ');
                    try self.writeOptionallyTypedIdent(tree, typed_ident.name, typed_ident.type, n.initializer);
                },
                .type_alias => |n| {
                    try self.writeTokenSpace(.k_alias);
                    try self.writeIdentifier(tree, n.new_name);
                    try self.writeTokenSpaced(.@"=");
                    try self.writeIndex(tree, n.old_type);
                },
                .import => |n| {
                    if (!self.imports) return;
                    try self.writeTokenSpace(.k_import);
                    if (n.aliases != 0) {
                        try self.writeList(tree, n.aliases, .{ .start = "{ ", .sep = "\n", .end = " }" });
                        try self.writeTokenSpaced(.k_from);
                    }
                    const extra = tree.extraData(node_mod.Import, n.import);
                    try self.writeByte('"');
                    try self.writeIdentifier(tree, extra.module);
                    try self.writeByte('"');
                },
                .@"struct" => |n| {
                    try self.writeTokenSpace(.k_struct);
                    try self.writeIdentifier(tree, n.name);
                    try self.writeSpace();
                    try self.startBlock();
                    try self.writeList(tree, n.members, .{ .sep = ",\n" });
                    try self.endBlock();
                },
                .struct_member => |n| {
                    const typed_ident = tree.extraData(node_mod.TypedIdent, n.typed_ident);
                    try self.writeAttributes(tree, n.attributes);
                    try self.writeOptionallyTypedIdent(tree, typed_ident.name, typed_ident.type, 0);
                },
                .import_alias => |n| {
                    try self.writeIdentifier(tree, n.old);
                    if (n.new != 0) {
                        try self.writeByte(' ');
                        try self.writeTokenSpace(.k_as);
                        try self.writeByte(' ');
                        try self.writeIdentifier(tree, n.new);
                    }
                },
                .fn_param => |n| {
                    const param = tree.extraData(node_mod.FnParam, n.fn_param);
                    try self.writeAttributes(tree, n.attributes);
                    try self.writeIdentifier(tree, param.name);
                    try self.writeArgType(tree, param.type);
                },
                .type, .ident => |n| {
                    try self.writeIdentifier(tree, n.name);
                    try self.writeTemplateList(tree, n.template_list);
                },
                .const_assert => |n| {
                    try self.writeTokenSpace(.k_const_assert);
                    try self.writeIndex(tree, n.expr);
                },
                .paren => |n| {
                    try self.writeToken(.@"(");
                    try self.writeIndex(tree, n.expr);
                    try self.writeToken(.@")");
                },
                .number => |n| try self.writeIdentifier(tree, n.value),
                .attribute => |a| {
                    try self.writeToken(.@"@");
                    try self.writeAll(@tagName(a));
                    switch (a) {
                        .compute, .@"const", .fragment, .invariant, .must_use, .vertex => {},
                        .@"align", .binding, .builtin, .group, .id, .location, .size => |e| {
                            try self.writeToken(.@"(");
                            try self.writeIndex(tree, e);
                            try self.writeToken(.@")");
                        },
                        .diagnostic => |d| try self.writeDiagnostic(tree, d),
                        .interpolate => |i| {
                            const interpolation = tree.extraData(node_mod.Interpolation, i);
                            try self.writeIndices(
                                false,
                                tree,
                                &.{ interpolation.type, interpolation.sampling_expr },
                                .{ .start = "(", .sep = ",", .end = ")" },
                            );
                        },
                        .workgroup_size => |w| {
                            const workgroup_size = tree.extraData(node_mod.WorkgroupSize, w);
                            var indices: []const Node.Index = &.{ workgroup_size.x, workgroup_size.y, workgroup_size.z };
                            var len: usize = 1;
                            if (workgroup_size.y != 0) len += 1;
                            if (workgroup_size.z != 0) len += 1;
                            try self.writeIndices(
                                false,
                                tree,
                                indices[0..len],
                                .{ .start = "(", .sep = ",", .end = ")" },
                            );
                        },
                    }
                },
                .loop => |n| {
                    try self.writeToken(.k_loop);
                    try self.writeAttributes(tree, n.attributes);
                    try self.writeSpace();
                    try self.writeIndex(tree, n.body);
                },
                .compound => |n| {
                    try self.writeAttributes(tree, n.attributes);
                    const indices = tree.spanToList(if (n.statements == 0) null else n.statements);
                    if (indices.len == 0) {
                        try self.writeToken(.@"{");
                        try self.writeToken(.@"}");
                        return;
                    }
                    if (n.attributes != 0) try self.writeSpace();
                    try self.startBlock();
                    for (indices, 0..) |index, i| {
                        try self.writeIndex(tree, index);
                        switch (tree.node(index)) {
                            .loop, .compound, .@"for", .@"if", .else_if, .@"else", .@"switch", .@"while", .continuing => {},
                            else => try self.writeToken(.@";"),
                        }
                        if (i != indices.len - 1) try self.newline();
                    }
                    try self.endBlock();
                },
                .@"for" => |n| {
                    const header = tree.extraData(node_mod.ForHeader, n.for_header);
                    try self.writeAttributes(tree, header.attrs);
                    try self.writeToken(.k_for);
                    try self.writeSpace();
                    try self.writeIndices(
                        false,
                        tree,
                        &.{ header.init, header.cond, header.update },
                        .{ .start = "(", .sep = ";", .sep_space = true, .end = ")" },
                    );
                    try self.writeSpace();
                    try self.writeIndex(tree, n.body);
                },
                .@"if" => |n| {
                    try self.writeTokenSpace(.k_if);
                    try self.writeIndex(tree, n.condition);
                    try self.writeSpace();
                    try self.writeIndex(tree, n.body);
                },
                .else_if => |n| {
                    try self.writeIndex(tree, n.if1);
                    try self.writeSpace();
                    try self.writeTokenSpace(.k_else);
                    try self.writeIndex(tree, n.if2);
                },
                .@"else" => |n| {
                    try self.writeIndex(tree, n.@"if");
                    try self.writeSpace();
                    try self.writeToken(.k_else);
                    try self.writeSpace();
                    try self.writeIndex(tree, n.body);
                },
                .@"switch" => |n| {
                    try self.writeTokenSpace(.k_switch);
                    try self.writeIndex(tree, n.expr);
                    try self.writeIndex(tree, n.switch_body);
                },
                .switch_body => |n| {
                    try self.writeAttributes(tree, n.attributes);
                    try self.writeSpace();
                    try self.startBlock();
                    try self.writeList(tree, n.clauses, .{ .sep = "\n" });
                    try self.endBlock();
                },
                .case_clause => |n| {
                    if (n.selectors != 0) {
                        try self.writeTokenSpace(.k_case);
                        try self.writeList(tree, n.selectors, .{ .sep = ",", .sep_space = true });
                    } else {
                        try self.writeToken(.k_default);
                    }
                    try self.writeSpace();
                    try self.writeIndex(tree, n.body);
                },
                .case_selector => |n| {
                    if (n.expr == 0) {
                        try self.writeToken(.k_default);
                    } else {
                        try self.writeIndex(tree, n.expr);
                    }
                },
                .@"while" => |n| {
                    try self.writeTokenSpace(.k_while);
                    try self.writeIndex(tree, n.condition);
                    try self.writeSpace();
                    try self.writeIndex(tree, n.body);
                },
                .@"return" => |n| {
                    try self.writeToken(.k_return);
                    if (n.expr != 0) {
                        try self.writeByte(' ');
                        try self.writeIndex(tree, n.expr);
                    }
                },
                .call => |n| {
                    try self.writeIndex(tree, n.ident);
                    const indices = tree.spanToList(if (n.arguments == 0) null else n.arguments);
                    try self.writeIndices(false, tree, indices, .{ .start = "(", .sep = ",", .sep_space = true, .end = ")" });
                },
                .@"var" => |n| {
                    const extra = tree.extraData(node_mod.Var, n.@"var");
                    try self.writeToken(.k_var);
                    try self.writeTemplateList(tree, extra.template_list);
                    try self.writeByte(' ');
                    try self.writeOptionallyTypedIdent(tree, extra.name, extra.type, n.initializer);
                },
                .increment, .decrement => |n| {
                    try self.writeIndex(tree, n.expr);
                    const op = switch (node) {
                        .increment => Token.@"++",
                        .decrement => Token.@"--",
                        else => unreachable,
                    };
                    try self.writeToken(op);
                },
                .phony_assign => |n| {
                    try self.writeToken(._);
                    try self.writeTokenSpaced(.@"=");
                    try self.writeIndex(tree, n.expr);
                },
                .@"=", .@"+=", .@"-=", .@"*=", .@"/=", .@"%=", .@"&=", .@"|=", .@"^=", .@"<<=", .@">>=" => |n| {
                    try self.writeIndex(tree, n.lhs_expr);
                    try self.writeSpaced(@tagName(node));
                    try self.writeIndex(tree, n.rhs_expr);
                },
                .continuing => |n| {
                    try self.writeToken(.k_continuing);
                    try self.writeSpace();
                    try self.writeIndex(tree, n.body);
                },
                .break_if => |n| {
                    try self.writeTokenSpace(.k_break);
                    try self.writeTokenSpace(.k_if);
                    try self.writeIndex(tree, n.expr);
                },
                .logical_not, .bitwise_complement, .negative, .deref, .ref => |n| {
                    const token: Token = switch (node) {
                        .logical_not => .@"!",
                        .bitwise_complement => .@"~",
                        .negative => .@"-",
                        .deref => .@"*",
                        .ref => .@"&",
                        else => unreachable,
                    };
                    try self.writeToken(token);
                    try self.writeIndex(tree, n.expr);
                },
                .lshift, .rshift => |n| {
                    try self.writeOp(tree, node, n.lhs_unary_expr, n.rhs_unary_expr);
                },
                .lt, .gt, .lte, .gte, .eq, .neq => |n| {
                    try self.writeOp(tree, node, n.lhs_shift_expr, n.rhs_shift_expr);
                },
                .mul, .div, .mod => |n| {
                    try self.writeOp(tree, node, n.lhs_multiplicative_expr, n.rhs_unary_expr);
                },
                .add, .sub => |n| {
                    try self.writeOp(tree, node, n.lhs_additive_expr, n.rhs_mul_expr);
                },
                .logical_and, .logical_or => |n| {
                    try self.writeOp(tree, node, n.lhs_relational_expr, n.rhs_relational_expr);
                },
                .bitwise_and, .bitwise_or, .bitwise_xor => |n| {
                    try self.writeOp(tree, node, n.lhs_bitwise_expr, n.rhs_unary_expr);
                },
                .field_access => |n| {
                    try self.writeIndex(tree, n.lhs_expr);
                    try self.writeToken(.@".");
                    try self.writeIdentifier(tree, n.member);
                },
                .index_access => |n| {
                    try self.writeIndex(tree, n.lhs_expr);
                    try self.writeToken(.@"[");
                    try self.writeIndex(tree, n.index_expr);
                    try self.writeToken(.@"]");
                },
                .true, .false, .@"break", .@"continue", .discard => try self.writeAll(@tagName(node)),
            }
        }

        fn startBlock(self: *Self) RetType {
            try self.writeToken(.@"{");
            self.pushIndent();
            try self.newline();
        }

        fn endBlock(self: *Self) RetType {
            self.popIndent();
            try self.newline();
            try self.writeToken(.@"}");
        }

        fn writeToken(self: *Self, token: Token) RetType {
            try self.writeAll(token.symbol());
        }

        fn writeTokenSpace(self: *Self, token: Token) RetType {
            try self.writeAll(token.symbol());
            try self.writeByte(' ');
        }

        fn writeTokenSpaced(self: *Self, token: Token) RetType {
            try self.writeSpace();
            try self.writeAll(token.symbol());
            try self.writeSpace();
        }

        fn writeOp(self: *Self, tree: Ast, node: Node, lhs: Node.Index, rhs: Node.Index) RetType {
            const token: Token = switch (node) {
                .lt => .@"<",
                .gt => .@">",
                .lte => .@"<=",
                .gte => .@">=",
                .eq => .@"==",
                .neq => .@"!=",
                .mul => .@"*",
                .div => .@"/",
                .mod => .@"%",
                .add => .@"+",
                .sub => .@"-",
                .logical_and => .@"&&",
                .logical_or => .@"||",
                .bitwise_and => .@"&",
                .bitwise_or => .@"|",
                .bitwise_xor => .@"^",
                .lshift => .@"<<",
                .rshift => .@">>",
                else => unreachable,
            };
            try self.writeIndex(tree, lhs);
            try self.writeTokenSpaced(token);
            try self.writeIndex(tree, rhs);
        }

        fn writeIndex(self: *Self, tree: Ast, index: Node.Index) RetType {
            if (index == 0) return;
            return self.writeNode(tree, tree.node(index));
        }

        fn writeIndices(
            self: *Self,
            comptime is_ident: bool,
            tree: Ast,
            indices: []const Node.Index,
            opts: SpanOpts,
        ) RetType {
            try self.writeAll(opts.start);
            for (indices, 0..) |index, i| {
                if (is_ident) try self.writeIdentifier(tree, index) else try self.writeIndex(tree, index);
                if (i != indices.len - 1) {
                    try self.writeAll(opts.sep);
                    if (opts.sep_space) try self.writeSpace();
                }
            }
            try self.writeAll(opts.end);
        }

        fn writeSpan(
            self: *Self,
            comptime is_ident: bool,
            tree: Ast,
            span: Node.Index,
            opts: SpanOpts,
        ) RetType {
            const indices = tree.spanToList(span);
            try self.writeIndices(is_ident, tree, indices, opts);
        }

        fn writeIdentList(self: *Self, tree: Ast, index: Node.Index, opts: SpanOpts) RetType {
            if (index == 0) return;
            try self.writeSpan(true, tree, index, opts);
        }

        fn writeList(self: *Self, tree: Ast, index: Node.Index, opts: SpanOpts) RetType {
            if (index == 0) return;
            try self.writeSpan(false, tree, index, opts);
        }

        fn writeIdentifier(self: *Self, tree: Ast, ident: Node.IdentIndex) RetType {
            try self.writeAll(tree.identifier(ident));
        }

        fn writeTemplateList(self: *Self, tree: Ast, index: Node.Index) RetType {
            try self.writeList(tree, index, .{ .start = "<", .sep = ",", .end = ">" });
        }

        fn writeAttributes(self: *Self, tree: Ast, index: Node.Index) RetType {
            if (index == 0) return;
            try self.writeList(tree, index, .{ .sep_space = true });
            try self.writeByte(' ');
        }

        fn writeArgType(self: *Self, tree: Ast, @"type": Node.Index) RetType {
            if (@"type" == 0) return;
            try self.writeToken(.@":");
            try self.writeSpace();
            try self.writeIndex(tree, @"type");
        }

        fn writeDiagnostic(self: *Self, tree: Ast, node: Node.ExtraIndex) RetType {
            try self.writeToken(.@"(");
            const diagnostic = tree.extraData(node_mod.DiagnosticControl, node);
            const sev: node_mod.Severity = @enumFromInt(diagnostic.severity);
            try self.writeAll(@tagName(sev));
            try self.writeToken(.@",");
            try self.writeIdentifier(tree, diagnostic.name);
            if (diagnostic.field != 0) {
                try self.writeToken(.@".");
                try self.writeIdentifier(tree, diagnostic.field);
            }
            try self.writeToken(.@")");
        }

        fn writeOptionallyTypedIdent(
            self: *Self,
            tree: Ast,
            name: Node.IdentIndex,
            @"type": Node.Index,
            initializer: Node.Index,
        ) RetType {
            try self.writeIdentifier(tree, name);
            try self.writeArgType(tree, @"type");
            if (initializer != 0) {
                try self.writeTokenSpaced(.@"=");
                try self.writeIndex(tree, initializer);
            }
        }
    };
}

fn testWrite(
    allocator: std.mem.Allocator,
    writer: anytype,
    source: [:0]const u8,
    minify: bool,
    imports: bool,
) !void {
    var tree = try Parser.parse(allocator, source);
    defer tree.deinit(allocator);

    if (tree.hasError()) {
        const stderr = std.io.getStdErr();
        const term = std.io.tty.detectConfig(stderr);
        try stderr.writer().writeByte('\n');
        try tree.renderErrors(stderr.writer(), term, source, null);
    } else {
        var renderer = Renderer(@TypeOf(writer)).init(writer, minify, imports);
        try renderer.writeTranslationUnit(tree);
    }
}

fn testRender(
    comptime source: [:0]const u8,
    comptime expected: [:0]const u8,
    minify: bool,
    imports: bool,
) !void {
    const allocator = std.testing.allocator;
    var arr = try std.ArrayList(u8).initCapacity(allocator, source.len);
    defer arr.deinit();

    try testWrite(allocator, arr.writer(), source, minify, imports);
    try std.testing.expectEqualStrings(expected, arr.items);
}

fn testCanonical(comptime source: [:0]const u8) !void {
    try testRender(source, source, false, false);
}

test "var" {
    try testCanonical("var a: u32 = 0u;");
    try testCanonical("var<uniform> a: u32 = 0u;");
    try testCanonical("@group(g_scene) @binding(0) var<uniform> view: View;");
}

test "const" {
    try testCanonical("const a: u32 = 0u;");
}

test "const assert" {
    try testCanonical("const_assert true;");
    try testCanonical("const_assert (true);");
}

test "comments" {
    try testRender(
        \\// Hello there
        \\const a: u32 = 0u;
        \\// Goodbye there
        \\const b: u32 = 1u;
        \\/* Block here */
        \\const c: u32 = 2u;
    ,
        \\const a: u32 = 0u;
        \\const b: u32 = 1u;
        \\const c: u32 = 2u;
    , false, false);
}

test "diagnostic" {
    try testCanonical("diagnostic(warning,foo);");
    try testCanonical("diagnostic(error,foo.bar);");
}

test "directives and declarations" {
    try testCanonical(
        \\enable foo,bar;
        \\requires feat1,feat2;
        \\diagnostic(off,derivative_uniformity);
        \\const a = 0.0;
    );
}

test "enable" {
    try testCanonical("enable f16;");
    try testCanonical("enable f16,f16;");
}

test "requires" {
    try testCanonical("requires foo;");
    try testCanonical("requires foo,bar;");
}

test "fn" {
    try testCanonical("fn main() {}");
    try testCanonical("fn main(a: u32) {}");
    try testCanonical("fn main(a: u32) -> u32 {}");
    try testCanonical("@vertex fn main() -> @location(0) u32 {}");
}

test "if" {
    try testCanonical(
        \\fn test() {
        \\  if !false {
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

test "attributes" {
    try testCanonical("@location(0) var a: u32;");
    try testCanonical("@interpolate(perspective,center) var a: u32;");
    try testCanonical("@workgroup_size(1) var a: u32;");
    try testCanonical("@workgroup_size(1,2,3) var a: u32;");
}

test "call" {
    try testCanonical("const b: Foo = Foo();");
}

test "struct" {
    const foo =
        \\struct Foo {
        \\  bar: u32,
        \\  baz: i32
        \\}
    ;
    try testCanonical(foo);

    try testRender(foo ++ ";", foo, false, false);

    try testCanonical(
        \\struct Foo {
        \\  @align(16) @size(4) bar: u32,
        \\  baz: i32
        \\}
    );

    try testCanonical(
        \\struct VertexInput {
        \\  @builtin(vertex_index) vertex: u32,
        \\  @builtin(instance_index) instance: u32
        \\}
        \\struct VertexOutput {
        \\  @builtin(position) position: vec4f,
        \\  @location(0) color: vec4f,
        \\  @location(1) worldPos: vec4f,
        \\  @location(2) normal: vec3f
        \\}
    );
}

test "override" {
    try testCanonical("@id(0) override wireframe: bool = false;");
}

test "type alias" {
    try testCanonical("alias single = f32;");
}

test "field access" {
    try testCanonical("var a = b.c[d].e;");
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

test "for" {
    try testCanonical(
        \\fn view32() -> @location(0) mat4x4f {
        \\  var a: i32 = 2;
        \\  for (var i: i32 = 0; i < 4; i++) {
        \\    if a == 0 {
        \\      continue;
        \\    }
        \\    a += 2;
        \\  }
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
        \\    case 3, 4, default {
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

test "while" {
    try testCanonical(
        \\fn test() {
        \\  while false {
        \\    return;
        \\  }
        \\}
    );
}

test "import" {
    const source = "// import { Foo } from \"./foo.wgsl\";";
    try testRender(source, source, false, true);
}

test "scope" {
    try testCanonical(
        \\fn test() {
        \\  {
        \\    return;
        \\  }
        \\}
    );
}

test "minify" {
    try testRender("var a: u32 = 0u;", "var a:u32=0u;", true, false);
    try testRender("var<uniform> a: u32 = 0u;", "var<uniform>a:u32=0u;", true, false);
    try testRender(
        \\fn test() {
        \\  while false {
        \\    if (true) {return;}
        \\  }
        \\}
    ,
        \\fn test(){while false{if (true){return;}}}
    , true, false);
}
