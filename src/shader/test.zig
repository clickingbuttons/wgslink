const std = @import("std");
const ErrorList = @import("ErrorList.zig");
const Ast = @import("Ast.zig");
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const allocator = std.testing.allocator;

fn expectAst(source: [:0]const u8) !void {
    var errors = try ErrorList.init(allocator);
    defer errors.deinit();

    var tree = Ast.parse(allocator, &errors, source) catch |err| {
        if (err == error.Parsing) {
            try errors.print(source, null);
        }
        return err;
    };
    defer tree.deinit(allocator);
}

test "boids-sprite" {
    const boids_sprite = @embedFile("test/boids-sprite.wgsl");
    try expectAst(boids_sprite);
}

test "boids-sprite-update" {
    const boids_sprite_update = @embedFile("test/boids-sprite-update.wgsl");
    try expectAst(boids_sprite_update);
}

test "cube-map" {
    const cube_map = @embedFile("test/cube-map.wgsl");
    try expectAst(cube_map);
}

test "fractal-cube" {
    const fractal_cube = @embedFile("test/fractal-cube.wgsl");
    try expectAst(fractal_cube);
}

test "gen-texture-light" {
    const gen_texture_light = @embedFile("test/gen-texture-light.wgsl");
    try expectAst(gen_texture_light);
}

test "gen-texture-light-cube" {
    const gen_texture_light_cube = @embedFile("test/gen-texture-light-cube.wgsl");
    try expectAst(gen_texture_light_cube);
}

test "sprite2d" {
    const sprite2d = @embedFile("test/sprite2d.wgsl");
    try expectAst(sprite2d);
}

test "two-cubes" {
    const two_cubes = @embedFile("test/two-cubes.wgsl");
    try expectAst(two_cubes);
}

test "fullscreen-textured-quad" {
    const fullscreen_textured_quad = @embedFile("test/fullscreen-textured-quad.wgsl");
    try expectAst(fullscreen_textured_quad);
}

test "image-blur" {
    const image_blur = @embedFile("test/image-blur.wgsl");
    try expectAst(image_blur);
}

test "instanced-cube" {
    const instanced_cube = @embedFile("test/instanced-cube.wgsl");
    try expectAst(instanced_cube);
}

test "map-async" {
    const map_async = @embedFile("test/map-async.wgsl");
    try expectAst(map_async);
}

test "pbr-basic" {
    const pbr_basic = @embedFile("test/pbr-basic.wgsl");
    try expectAst(pbr_basic);
}

test "pixel-post-process-normal-frag" {
    const pixel_post_process_normal_frag = @embedFile("test/pixel-post-process-normal-frag.wgsl");
    try expectAst(pixel_post_process_normal_frag);
}

test "pixel-post-process-pixel-vert" {
    const pixel_post_process_pixel_vert = @embedFile("test/pixel-post-process-pixel-vert.wgsl");
    try expectAst(pixel_post_process_pixel_vert);
}

test "pixel-post-process-pixel-frag" {
    const pixel_post_process_pixel_frag = @embedFile("test/pixel-post-process-pixel-frag.wgsl");
    try expectAst(pixel_post_process_pixel_frag);
}

test "pixel-post-process" {
    const pixel_post_process = @embedFile("test/pixel-post-process.wgsl");
    try expectAst(pixel_post_process);
}

test "procedural-primitives" {
    const procedural_primitives = @embedFile("test/procedural-primitives.wgsl");
    try expectAst(procedural_primitives);
}

test "rotating-cube" {
    const rotating_cube = @embedFile("test/rotating-cube.wgsl");
    try expectAst(rotating_cube);
}

test "triangle" {
    const triangle = @embedFile("test/triangle.wgsl");
    try expectAst(triangle);
}

test "fragmentDeferredRendering" {
    const fragmentDeferredRendering = @embedFile("test/fragmentDeferredRendering.wgsl");
    try expectAst(fragmentDeferredRendering);
}

test "fragmentGBuffersDebugView" {
    const fragmentGBuffersDebugView = @embedFile("test/fragmentGBuffersDebugView.wgsl");
    try expectAst(fragmentGBuffersDebugView);
}

test "fragmentWriteGBuffers" {
    const fragmentWriteGBuffers = @embedFile("test/fragmentWriteGBuffers.wgsl");
    try expectAst(fragmentWriteGBuffers);
}

test "lightUpdate" {
    const lightUpdate = @embedFile("test/lightUpdate.wgsl");
    try expectAst(lightUpdate);
}

test "vertexTextureQuad" {
    const vertexTextureQuad = @embedFile("test/vertexTextureQuad.wgsl");
    try expectAst(vertexTextureQuad);
}

test "vertexWriteGBuffers" {
    const vertexWriteGBuffers = @embedFile("test/vertexWriteGBuffers.wgsl");
    try expectAst(vertexWriteGBuffers);
}

