const std = @import("std");
const Ast = @import("./wgsl/Ast.zig");
const Node = @import("./wgsl/Node.zig");
const Token = @import("./wgsl/Token.zig");
const AstUtils = @import("./wgsl/AstUtils.zig");
const root_token_i = @import("./wgsl/Parser.zig").root_token_i;

const Allocator = std.mem.Allocator;

/// Renders bundles of modules back into WGSL.
pub fn Renderer(comptime UnderlyingWriter: type) type {
    return struct {
        const Self = @This();
        pub const WriteError = UnderlyingWriter.Error || error{OutOfMemory};
        pub const Writer = std.io.Writer(*Self, WriteError, write);
        const Idents = std.StringArrayHashMap(usize);

        underlying_writer: UnderlyingWriter,
        minify: bool,

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
        span_sep_needs_semi: bool = false,

        pub fn init(underlying_writer: UnderlyingWriter, minify: bool) Self {
            return Self{
                .underlying_writer = underlying_writer,
                .minify = minify,
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
            if (bytes.len == 0) return 0;
            if (self.disabled_offset == null) try self.underlying_writer.writeAll(bytes);
            if (bytes[bytes.len - 1] == '\n') self.resetLine();
            return bytes.len;
        }

        pub fn newline(self: *Self) WriteError!void {
            if (!self.minify) _ = try self.writeNoIndent("\n");
        }

        fn resetLine(self: *Self) void {
            self.current_line_empty = true;
            self.indent_next_line = 0;
        }

        /// Insert a newline unless the current line is blank
        pub fn maybeInsertNewline(self: *Self) WriteError!void {
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
        fn applyIndent(self: *Self) WriteError!void {
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

        pub fn writeTranslationUnit(self: *Self, tree: Ast) !void {
            try AstUtils.visit(.{ .ctx = self, .tree = tree, .visitor = visit }, 0);
        }

        fn writeNode(self: *Self, tree: Ast, node: Node.Index) !void {
            const src = tree.nodeSource(node);
            // std.debug.print("writeNode {any} {s}\n", .{ tree.nodeTag(node), src });
            try self.writeAll(src);
        }

        fn writeSpace(self: *Self) !void {
            if (!self.minify) try self.writeByte(' ');
        }

        fn writeListSep(self: *Self) !void {
            try self.writeByte(',');
            try self.writeSpace();
        }

        fn writeSpaced(self: *Self, token: []const u8) !void {
            try self.writeSpace();
            try self.writeAll(token);
            try self.writeSpace();
        }

        fn visit(ctx: anytype, visit_data: AstUtils.VisitData) WriteError!void {
            const self: *Self = ctx.ctx;
            const tree = ctx.tree;
            switch (visit_data) {
                .node => |n| {
                    const tag = tree.nodeTag(n);
                    switch (tag) {
                        .global_var => {
                            const data = tree.nodeData(Node.Data.GlobalVar, n);
                            const global_var = tree.extraData(Node.GlobalVar, data.global_var);
                            try self.writeNode(tree, n);
                            if (global_var.template_list == 0) try self.writeByte(' ');
                        },
                        .enable_directive,
                        .requires_directive,
                        .@"fn",
                        .@"const",
                        .const_assert,
                        .@"struct",
                        .override,
                        => {
                            try self.writeNode(tree, n);
                            try self.writeByte(' ');
                        },
                        .compound => {
                            try self.writeSpace();
                            self.pushIndentNextLine();
                            try self.writeNode(tree, n);
                        },
                        .attr => {
                            const attr = tree.nodeData(Node.Data.Attr, n);
                            try self.writeNode(tree, n);
                            try self.writeAll(@tagName(attr.tag));
                        },
                        else => try self.writeNode(tree, n),
                    }
                    switch (tag) {
                        .global_var,
                        .override,
                        .@"const",
                        .type_alias,
                        .const_assert,
                        .diagnostic_directive,
                        .enable_directive,
                        .requires_directive,
                        => self.span_sep_needs_semi = true,
                        .@"struct", .@"fn" => self.span_sep_needs_semi = false,
                        else => {},
                    }
                },
                .token => |t| {
                    const token = tree.tokenSource(t);
                    try self.writeAll(token);
                },
                .token_tag => |token| {
                    switch (token) {
                        .@"=", .@"->" => try self.writeSpaced(@tagName(token)),
                        .@":" => {
                            try self.writeAll(@tagName(token));
                            try self.writeSpace();
                        },
                        else => {
                            try self.writeAll(@tagName(token));
                        },
                    }
                },
                .span_start => |span_start| {
                    if (span_start == root_token_i) return;
                    switch (tree.tokenTag(span_start)) {
                        .@"<", .template_args_start, .@"(" => {
                            const token = tree.tokenSource(span_start);
                            try self.writeAll(token);
                        },
                        .k_struct => {
                            self.pushIndentNextLine();
                            try self.writeSpace();
                            try self.writeAll("{\n");
                        },
                        .k_enable, .k_requires, .@"@" => {},
                        else => |t| std.debug.print("unknown span start {any}\n", .{t}),
                    }
                },
                .span_sep => |span_start| {
                    if (span_start == root_token_i) {
                        if (self.span_sep_needs_semi) try self.writeByte(';');
                        try self.newline();
                        return;
                    }
                    switch (tree.tokens.items(.tag)[span_start]) {
                        .@"<", .template_args_start => try self.writeByte(','),
                        .@"(", .k_enable, .k_requires => try self.writeListSep(),
                        .k_struct => {
                            try self.writeByte(',');
                            try self.newline();
                        },
                        .@"@" => try self.writeSpace(),
                        else => |t| std.debug.print("unknown span sep {any}\n", .{t}),
                    }
                },
                .span_end => |span_start| {
                    if (span_start == root_token_i) {
                        if (self.span_sep_needs_semi) try self.writeByte(';');
                        return;
                    }
                    switch (tree.tokenTag(span_start)) {
                        .@"<", .template_args_start => {
                            try self.writeByte('>');
                            if (span_start > 0 and tree.tokenTag(span_start - 1) == .k_var) {
                                try self.writeSpace();
                            }
                        },
                        .@"(" => try self.writeByte(')'),
                        .@"@" => try self.writeByte(' '),
                        .k_enable, .k_requires => {},
                        .k_struct => {
                            self.popIndent();
                            try self.writeAll("\n}");
                        },
                        else => |t| {
                            std.debug.print("unknown span end {any}\n", .{t});
                        },
                    }
                },
            }
        }
    };
}

fn testWrite(
    allocator: std.mem.Allocator,
    writer: anytype,
    source: [:0]const u8,
    minify: bool,
) !void {
    var tree = try Ast.init(allocator, source);
    defer tree.deinit(allocator);
    const stderr = std.io.getStdErr();
    const term = std.io.tty.detectConfig(stderr);
    if (tree.errors.len > 0) try stderr.writer().writeByte('\n');
    for (tree.errors) |e| try tree.renderError(e, stderr.writer(), term, null);
    if (tree.errors.len == 0) {
        var renderer = Renderer(@TypeOf(writer)).init(writer, minify);
        try renderer.writeTranslationUnit(tree);
    }
}

fn testRender(comptime source: [:0]const u8, comptime expected: [:0]const u8, minify: bool) !void {
    const allocator = std.testing.allocator;
    var arr = try std.ArrayList(u8).initCapacity(allocator, source.len);
    defer arr.deinit();

    try testWrite(allocator, arr.writer(), source, minify);
    try std.testing.expectEqualStrings(expected, arr.items);
}

fn testCanonical(comptime source: [:0]const u8) !void {
    try testRender(source, source, false);
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
    , false);
}

test "enable" {
    try testCanonical("enable f16;");
    try testCanonical("enable f16, f16;");
}

test "diagnostic" {
    try testCanonical("diagnostic(warning, foo);");
    try testCanonical("diagnostic(error, foo.bar);");
}

test "fn" {
    try testCanonical("fn main() {}");
    try testCanonical("fn main(a: u32) {}");
    try testCanonical("fn main(a: u32) -> u32 {}");
    try testCanonical("@vertex fn main() -> @location(0) u32 {}");
}

test "attributes" {
    try testCanonical("@location(0) var a: u32;");
    try testCanonical("@interpolate(perspective, center) var a: u32;");
    try testCanonical("@workgroup_size(1, 2, 3) var a: u32;");
}

test "requires" {
    try testCanonical("requires foo;");
    try testCanonical("requires foo, bar;");
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

    try testRender(foo ++ ";", foo, false);

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

// test "type alias" {
//     try testCanonical("alias single = f32;");
// }
//
// test "field access" {
//     try testCanonical("var a = b.c[d].e;");
// }
//

// test "function" {
//     try testCanonical(
//         \\fn view32() -> @location(0) mat4x4f {
//         \\  var res = mat4x4f(view.view);
//         \\  res[3][0] = 0.0;
//         \\  res[3][1] = 0.0;
//         \\  res[3][2] = 0.0;
//         \\  return res;
//         \\}
//     );
// }
//
// test "entry function" {
//     try testCanonical("@vertex fn main() {}");
//     try testCanonical("@compute @workgroup_size(1,2,3) fn main() {}");
// }
//
// test "paren" {
//     try testCanonical("const a = (3 - 2);");
//     try testCanonical("const a = ((3 - 2));");
// }
//
// test "switch" {
//     try testCanonical(
//         \\fn test() {
//         \\  switch a {
//         \\    case 3, 4, default {
//         \\      b = 2;
//         \\      break;
//         \\    }
//         \\    default {
//         \\      break;
//         \\    }
//         \\  }
//         \\}
//     );
// }
//
// test "loop" {
//     try testCanonical(
//         \\fn test() {
//         \\  loop {
//         \\    if (i >= 4) {
//         \\      break;
//         \\    }
//         \\    i++;
//         \\  }
//         \\}
//     );
//
//     try testCanonical(
//         \\fn test() {
//         \\  loop {
//         \\    if i % 2 == 0 {
//         \\      continue;
//         \\    }
//         \\    continuing {
//         \\      i++;
//         \\      break if i >= 4;
//         \\    }
//         \\  }
//         \\}
//     );
// }
//
// test "if" {
//     try testCanonical(
//         \\fn test() {
//         \\  if false {
//         \\    return;
//         \\  }
//         \\}
//     );
//
//     try testCanonical(
//         \\fn test() {
//         \\  if false {
//         \\    return;
//         \\  } else if true {
//         \\    return;
//         \\  } else {
//         \\    return;
//         \\  }
//         \\}
//     );
// }
//
// test "while" {
//     try testCanonical(
//         \\fn test() {
//         \\  while false {
//         \\    return;
//         \\  }
//         \\}
//     );
// }
//
// test "import" {
//     try testRender("// import { Foo } from './foo.wgsl';", "", false);
// }
//
// test "minify" {
//     try testRender(
//         \\fn test() {
//         \\  while false {
//         \\    if (true) {return;}
//         \\  }
//         \\}
//     ,
//         \\fn test(){while false{if(true){return;}}}
//     , true);
// }
