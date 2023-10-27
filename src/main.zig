const std = @import("std");
const clap = @import("clap");

const ErrorList = @import("./shader/ErrorList.zig");
const Ast = @import("./shader/Ast.zig");
const AstGen = @import("./AstGen.zig");

const NodeIndex = Ast.NodeIndex;

fn writePretty(allocator: std.mem.Allocator, writer: anytype, source: [:0]const u8) !void {
    var errors = try ErrorList.init(allocator);
    defer errors.deinit();
    var ast = Ast.parse(allocator, &errors, source) catch |err| {
        if (err == error.Parsing) try errors.print(source, null);
        return err;
    };
    defer ast.deinit(allocator);
    // for (0..ast.nodes.len) |i| {
    //     const t: NodeIndex = @enumFromInt(i);
    //     const tok = ast.nodeToken(t);
    //     std.debug.print("{s} {s}\n", .{ @tagName(ast.nodeTag(t)), @tagName(ast.tokenTag(tok)) });
    // }

    const generator = AstGen{
        .tree = &ast,
        .tab = "  ",
    };
    try generator.writeTranslationUnit(writer);
}

pub fn main() !void {
    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit.
        \\<str>...
        \\
    );
    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{ .diagnostic = &diag }) catch |err| {
        diag.report(std.io.getStdErr().writer(), err) catch {};
        return err;
    };
    defer res.deinit();

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const maxSize = 1 << 30;
    for (res.positionals) |pos| {
        const file = try std.fs.cwd().openFile(pos, .{});
        defer file.close();

        const source = try file.readToEndAllocOptions(allocator, maxSize, null, @alignOf(u8), 0);
        defer allocator.free(source);

        try writePretty(allocator, stdout, source);
    }
    if (res.positionals.len == 0) {
        const source = try std.io.getStdIn().readToEndAllocOptions(allocator, maxSize, null, @alignOf(u8), 0);
        defer allocator.free(source);

        try writePretty(allocator, stdout, source);
    }

    try bw.flush();
}

fn testPretty(source: [:0]const u8, expected: ?[:0]const u8) !void {
    const allocator = std.testing.allocator;
    var arr = try std.ArrayList(u8).initCapacity(allocator, source.len);
    defer arr.deinit();

    try writePretty(allocator, arr.writer(), source);

   try std.testing.expectEqualStrings(expected orelse source, arr.items[0..arr.items.len - 1]);
}

fn testSame(source: [:0]const u8) !void {
    try testPretty(source, null);
}

test "array type" {
    try testSame("var a: array<u32,3> = array<u32,3>(0u, 1u, 2u);");
}

test "const" {
    try testSame("const a: u32 = 0u;");
    try testSame("const b: Foo = Foo();");
    try testSame("const c: vec2f = vec2f();");
    try testSame("const d: vec2f = vec2f(1.0);");
}

test "struct" {
    try testSame(
        \\struct Foo {
        \\  bar: u32,
        \\  baz: i32,
        \\}
    );

    try testSame(
        \\struct Foo {
        \\  @align(16) @size(4) bar: u32,
        \\  baz: i32,
        \\}
    );

    try testSame(
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
    try testSame("@group(g_scene) @binding(0) var<uniform> view: View;");
}

test "var types" {
    try testSame("var a: vec2<Test>;");
    try testSame("var b: array<i32>;");
    try testSame("var b: mat4x4<f32>;");
    try testSame("var b: mat4x4f;");

    try testSame("var ptr_int: ptr<function,i32>;");
    try testSame("var ptr_int2: ptr<function,i32,read>;");

    try testSame("var sam: sampler;");
    try testSame("var tex: texture_2d<f32>;");

    try testSame("var tex2: texture_multisampled_2d<i32>;");
    try testSame("var tex3: texture_depth_multisampled_2d;");

    try testSame("var tex4: texture_storage_2d<rgba8unorm,read>;");

    try testSame("var tex5: texture_depth_2d;");
    try testSame("var tex6: texture_external;");
}

test "override" {
    try testSame("@id(0) override wireframe: bool = false;");
}

test "type alias" {
    try testSame("alias single = f32;");
}

test "field access" {
    try testSame("var a = b.c;");
}

test "comments" {
    try testPretty(
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
    try testSame(
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
    try testSame("@vertex fn main() {}");
    try testSame("@compute @workgroup_size(1,2,3) fn main() {}");
}

test "paren" {
    try testSame("const a = (3 - 2);");
    try testSame("const a = ((3 - 2));");
}

test "test files" {
    _ = @import("./shader/test.zig");
}
