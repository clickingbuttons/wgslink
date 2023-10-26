const std = @import("std");

const ErrorList = @import("./shader/ErrorList.zig");
const Ast = @import("./shader/Ast.zig");
const AstGen = @import("./AstGen.zig");

const NodeIndex = Ast.NodeIndex;

fn writePretty(allocator: std.mem.Allocator, writer: anytype, source: [:0]const u8) !void {
    var errors = try ErrorList.init(allocator);
    defer errors.deinit();
    var ast = Ast.parse(allocator, &errors, source) catch |err| {
        if (err == error.Parsing) {
            try errors.print(source, null);
        }
        return err;
    };
    defer ast.deinit(allocator);

    const generator = AstGen{
        .tree = &ast,
        .tab = "  ",
    };
    try generator.writeTranslationUnit(writer);
}

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const source =
        \\ // Whitepaper: https://andrewthall.org/papers/dfp64_qf128.pdf
        \\ // WGSL port of https://github.com/visgl/luma.gl/blob/291a2fdfb1cfdb15405032b3dcbfbe55133ead61/modules/shadertools/src/modules/math/fp64/fp64-arithmetic.glsl.ts
        \\
        \\ struct fp64 {
        \\ 	high: f32,
        \\ 	low: f32,
        \\ }
        \\
    ;

    try writePretty(allocator, stdout, source);

    try bw.flush();
}

fn testSame(source: [:0]const u8) !void {
    const allocator = std.testing.allocator;
    var arr = try std.ArrayList(u8).initCapacity(allocator, source.len);
    defer arr.deinit();

    try writePretty(allocator, arr.writer(), source);

    try std.testing.expectEqualStrings(source, arr.items);
}

test "array type" {
    try testSame("var a: array<u32,3> = array<u32,3>(0u, 1u, 2u);\n");
}

test "const" {
    try testSame("const a: u32 = 0u;\n");
    try testSame("const b: Foo = Foo();\n");
    try testSame("const c: vec2f = vec2f();\n");
    try testSame("const d: vec2f = vec2f(1.0);\n");
}

test "struct" {
    try testSame(
        \\struct Foo {
        \\  bar: u32,
        \\  baz: i32,
        \\}
        \\
    );

    try testSame(
        \\struct Foo {
        \\  @align(16) @size(4) bar: u32,
        \\  baz: i32,
        \\}
        \\
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
        \\
    );
}

test "global var" {
    try testSame("@group(g_scene) @binding(0) var<uniform> view: View;\n");
}

test "var types" {
    try testSame("var a: vec2<Test>;\n");
    try testSame("var b: array<i32>;\n");
    try testSame("var b: mat4x4<f32>;\n");
    try testSame("var b: mat4x4f;\n");

    try testSame("var ptr_int: ptr<function,i32>;\n");
    try testSame("var ptr_int2: ptr<function,i32,read>;\n");

    try testSame("var sam: sampler;\n");
    try testSame("var tex: texture_2d<f32>;\n");

    try testSame("var tex2: texture_multisampled_2d<i32>;\n");
    try testSame("var tex3: texture_depth_multisampled_2d;\n");

    try testSame("var tex4: texture_storage_2d<rgba8unorm,read>;\n");

    try testSame("var tex5: texture_depth_2d;\n");
    try testSame("var tex6: texture_external;\n");
}

test "override" {
    try testSame("@id(0) override wireframe: bool = false;\n");
}

test "type alias" {
    try testSame("alias single = f32;\n");
}

test "field access" {
    try testSame("var a = b.c;\n");
}

test "comments" {
    try testSame(
        \\// Hello there
        \\const a: u32 = 0u;
        \\// Goodbye there
        \\const b: u32 = 0u;
        \\/* Block here */
        \\const c: u32 = 0u;
        \\
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
        \\
    );
}

test "entry function" {
    try testSame("@vertex fn main() {}\n");
    try testSame("@compute @workgroup_size(1,2,3) fn main() {}\n");
}
