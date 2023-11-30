const std = @import("std");
const Module = @import("module.zig");
const Renderer = @import("./WgslRenderer.zig").Renderer;
const TreeShakeOptions = @import("./ast/TreeShaker.zig").Options;
const Ast = @import("./ast/Ast.zig");
const Aliaser = @import("./ast/Aliaser.zig");
const FileError = @import("./file/Error.zig");
const node_mod = @import("./ast/Node.zig");

const Self = @This();
const Allocator = std.mem.Allocator;
const ThreadPool = std.Thread.Pool;
const Mutex = std.Thread.Mutex;
const WaitGroup = std.Thread.WaitGroup;
const Modules = std.StringArrayHashMap(Module);
const Writer = @TypeOf(std.ArrayList(u8).writer());
const Visited = std.StringHashMap(void);

allocator: Allocator,
thread_pool: *ThreadPool,
modules: Modules,
modules_mutex: Mutex = .{},
stderr_mutex: Mutex = .{},

pub fn init(allocator: Allocator, thread_pool: *ThreadPool) !Self {
    return Self{
        .allocator = allocator,
        .thread_pool = thread_pool,
        .modules = Modules.init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    for (self.modules.values()) |*v| v.deinit();
    self.modules.deinit();
}

pub fn alias(self: Self, aliaser: *Aliaser, visited: *Visited, module: *Module) !void {
    if ((try visited.getOrPut(module.path)).found_existing) return;
    if (module.file) |*f| {
        for (f.import_table.keys()) |k| {
            const m = self.modules.getPtr(k).?;
            try self.alias(aliaser, visited, m);
        }
        try aliaser.appendComment(module.path);
        try aliaser.append(f.tree);
    }
}

pub const Options = struct {
    file: []const u8,
    tree_shake: ?TreeShakeOptions,
    minify: bool,
};

pub fn bundle(
    self: *Self,
    writer: anytype,
    errwriter: anytype,
    errconfig: std.io.tty.Config,
    opts: Options,
) !void {
    const root_mod = Module.init(self.allocator, opts.file, "bundle api");
    try self.modules.put(root_mod.path, root_mod);
    const mod = self.modules.getPtr(root_mod.path).?;

    var wait_group: WaitGroup = .{};
    wait_group.start();
    try self.thread_pool.spawn(workerAst, .{
        self,
        errwriter,
        errconfig,
        &wait_group,
        mod,
        opts.tree_shake,
    });
    wait_group.wait();
    var has_unparsed = false;
    for (self.modules.values()) |v| {
        if (v.hasError()) has_unparsed = true;
    }
    if (has_unparsed) return error.UnparsedModule;

    var visited = Visited.init(self.allocator);
    defer visited.deinit();
    var aliaser = try Aliaser.init(self.allocator, opts.minify);
    defer aliaser.deinit();
    try self.alias(&aliaser, &visited, mod);

    var tree = try aliaser.toOwnedAst(.wgsl);
    defer tree.deinit(self.allocator);
    var renderer = Renderer(@TypeOf(writer)).init(writer, opts.minify, false);
    try renderer.writeTranslationUnit(tree);
}

/// tokenize, parse and scan files for imports
fn workerAst(
    self: *Self,
    errwriter: anytype,
    errconfig: std.io.tty.Config,
    wait_group: *WaitGroup,
    mod: *Module,
    tree_shake: ?TreeShakeOptions,
) void {
    defer wait_group.finish();

    mod.load(tree_shake) catch |err| {
        self.stderr_mutex.lock();
        defer self.stderr_mutex.unlock();
        switch (err) {
            error.Parsing => mod.renderErrors(errwriter, errconfig) catch {},
            // Some kind of file error...
            else => |t| {
                const imp: Module = self.modules.get(mod.imported_by).?;
                // Assertion: we don't queue import table for modules with errors
                const file = imp.file.?;
                const tree: Ast = file.tree;
                for (tree.spanToList(0)) |i| {
                    switch (tree.node(i)) {
                        .import => |n| {
                            const mod_name = tree.modName(n);
                            const resolved = imp.resolve(mod_name) catch continue;
                            defer self.allocator.free(resolved);
                            if (std.mem.eql(u8, resolved, mod.path)) {
                                const extra = tree.extraData(node_mod.Import, n.import);
                                const error_loc = node_mod.ErrorLoc{
                                    .line_num = extra.line_num,
                                    .line_start = extra.line_start,
                                    .tok_start = extra.tok_start,
                                    .tok_end = extra.tok_end,
                                };
                                const source = imp.source() catch return;
                                defer self.allocator.free(source);
                                FileError.render(errwriter, errconfig, source, imp.path, error_loc) catch return;

                                errwriter.print("{s} opening file {s}", .{ @errorName(t), mod.path }) catch return;
                                errconfig.setColor(errwriter, .reset) catch return;
                                errwriter.writeByte('\n') catch return;
                            }
                        },
                        else => {},
                    }
                }
            },
        }
        return;
    };

    self.modules_mutex.lock();
    defer self.modules_mutex.unlock();
    var iter = mod.file.?.import_table.iterator();
    while (iter.next()) |kv| {
        const mod_name = kv.key_ptr.*;
        const mod_symbols = kv.value_ptr.*.keys();
        const gop = self.modules.getOrPut(mod_name) catch return;
        if (!gop.found_existing) {
            gop.value_ptr.* = Module.init(self.allocator, mod_name, mod.path);
            wait_group.start();
            const next_tree_shake = if (mod_symbols.len == 0) null else TreeShakeOptions{
                .symbols = mod_symbols,
                .find_symbols = false,
            };
            self.thread_pool.spawn(workerAst, .{ self, errwriter, errconfig, wait_group, gop.value_ptr, next_tree_shake }) catch {
                wait_group.finish();
                continue;
            };
        }
    }
}

fn testBundle(comptime entry: []const u8, comptime expected: [:0]const u8) !void {
    const allocator = std.testing.allocator;

    var thread_pool: ThreadPool = undefined;
    try thread_pool.init(.{ .allocator = allocator });
    defer thread_pool.deinit();

    var bundler = try Self.init(allocator, &thread_pool);
    defer bundler.deinit();

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var errbuf = std.ArrayList(u8).init(allocator);
    defer errbuf.deinit();

    try bundler.bundle(buffer.writer(), errbuf.writer(), .no_color, Options{
        .file = entry,
        .tree_shake = TreeShakeOptions{},
        .minify = false,
    });

    try std.testing.expectEqualStrings(expected, buffer.items);
}

test "basic bundle" {
    try testBundle("./test/bundle-basic/a.wgsl",
        \\// test/bundle-basic/c.wgsl
        \\const c = 3.0;
        \\// test/bundle-basic/b.wgsl
        \\const b = 2.0 + c;
        \\// test/bundle-basic/a.wgsl
        \\fn foo() -> f32 {
        \\  return 4.0;
        \\}
        \\const a = 1.0 + b + foo();
        \\@vertex fn main() -> @location(0) vec4f {
        \\  return vec4f(a);
        \\}
    );
}

test "cycle bundle" {
    try testBundle("./test/bundle-cycle/a.wgsl",
        \\// test/bundle-cycle/c.wgsl
        \\const c = 3.0 + a;
        \\// test/bundle-cycle/b.wgsl
        \\const b = 2.0 + c;
        \\// test/bundle-cycle/a.wgsl
        \\const a = 1.0 + b;
        \\@vertex fn main() -> @location(0) vec4f {
        \\  return vec4f(a);
        \\}
    );
}

test "type alias bundle" {
    try testBundle("./test/bundle-type-alias/a.wgsl",
        \\// test/bundle-type-alias/a.wgsl
        \\alias single = f32;
        \\const pi_approx: single = 3.1415;
        \\fn two_pi() -> single {
        \\  return single(2) * pi_approx;
        \\}
        \\@vertex fn main() -> @location(0) vec4f {
        \\  return vec4f(two_pi());
        \\}
    );
}

test "directive bundle" {
    try testBundle("./test/bundle-directive/a.wgsl",
        \\enable baz,quz,foo,bar;
        \\requires feat3,feat4,feat1,feat2;
        \\diagnostic(off,foo);
        \\diagnostic(off,derivative_uniformity);
        \\// test/bundle-directive/b.wgsl
        \\const b: f32 = 0.0;
        \\// test/bundle-directive/a.wgsl
        \\@vertex fn main() -> @location(0) vec4f {
        \\  return vec4f(b);
        \\}
    );
}

test "ident clash bundle" {
    try testBundle("./test/bundle-ident-clash/a.wgsl",
        \\// test/bundle-ident-clash/b.wgsl
        \\var a = 2.0;
        \\var b = a + 3.0;
        \\// test/bundle-ident-clash/a.wgsl
        \\var a2 = 1.0 + b;
        \\@vertex fn main() -> @location(0) vec4f {
        \\  return vec4f(a2);
        \\}
    );
}
