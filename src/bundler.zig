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
pub const Options = struct {
    tree_shake: ?TreeShakeOptions,
    minify: bool,
};

allocator: Allocator,
thread_pool: *ThreadPool,
modules: Modules,
opts: Options,
modules_mutex: Mutex = .{},
stderr_mutex: Mutex = .{},

pub fn init(allocator: Allocator, thread_pool: *ThreadPool, opts: Options) !Self {
    return Self{
        .allocator = allocator,
        .thread_pool = thread_pool,
        .modules = Modules.init(allocator),
        .opts = opts,
    };
}

pub fn deinit(self: *Self) void {
    for (self.modules.values()) |*v| v.deinit();
    self.modules.deinit();
}

pub fn alias(
    self: Self,
    errwriter: anytype,
    errconfig: std.io.tty.Config,
    aliaser: *Aliaser,
    visited: *Visited,
    module: Module,
) !void {
    if ((try visited.getOrPut(module.path)).found_existing) return;
    for (module.import_table.keys()) |k| {
        const m = self.modules.get(k).?;
        try self.alias(errwriter, errconfig, aliaser, visited, m);
    }
    if (!self.opts.minify) try aliaser.appendComment(module.path);

    const err_count = aliaser.builder.errors.items.len;
    const tree = module.file.tree.?;
    const source = module.file.source.?;
    try aliaser.append(tree);
    for (aliaser.builder.errors.items[err_count..]) |e| {
        try e.write(
            errwriter,
            errconfig,
            module.file.path,
            source,
            tree.getErrorLoc(source, e.src_offset),
        );
    }
}

pub fn bundle(
    self: *Self,
    writer: anytype,
    errwriter: anytype,
    errconfig: std.io.tty.Config,
    file: []const u8,
) !void {
    const resolved = try Module.resolveFrom(self.allocator, "./a", file);
    defer self.allocator.free(resolved);
    const root_mod = Module.init(self.allocator, resolved, "bundle api");
    try self.modules.put(resolved, root_mod);
    const mod = self.modules.getPtr(resolved).?;

    var wait_group: WaitGroup = .{};
    wait_group.start();
    try self.thread_pool.spawn(workerAst, .{
        self,
        errwriter,
        errconfig,
        &wait_group,
        mod,
        self.opts.tree_shake,
    });
    wait_group.wait();
    var has_unparsed = false;
    for (self.modules.values()) |v| {
        if (v.hasError()) has_unparsed = true;
    }
    if (has_unparsed) return error.UnparsedModule;

    var visited = Visited.init(self.allocator);
    defer visited.deinit();
    var aliaser = try Aliaser.init(self.allocator, self.opts.minify);
    defer aliaser.deinit();
    try self.alias(errwriter, errconfig, &aliaser, &visited, mod.*);
    var tree = try aliaser.toOwnedAst(.wgsl);
    defer tree.deinit(self.allocator);
    if (tree.errors.len > 0) return error.Aliasing;

    var renderer = Renderer(@TypeOf(writer)).init(writer, self.opts.minify, false);
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
            error.Parsing => mod.writeParsingErrors(errwriter, errconfig) catch {},
            // Probably some kind of file opening error...
            else => |t| self.writeModuleFileError(errwriter, errconfig, mod, @errorName(t)) catch {},
        }
        return;
    };

    self.modules_mutex.lock();
    defer self.modules_mutex.unlock();
    var iter = mod.import_table.iterator();
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
            self.thread_pool.spawn(workerAst, .{
                self,
                errwriter,
                errconfig,
                wait_group,
                gop.value_ptr,
                next_tree_shake,
            }) catch {
                wait_group.finish();
                continue;
            };
        }
    }
}

fn writeModuleFileError(
    self: Self,
    errwriter: anytype,
    errconfig: std.io.tty.Config,
    mod: *const Module,
    errname: []const u8,
) !void {
    const imp_by: Module = self.modules.get(mod.imported_by).?;
    // Assertion: we don't queue import table for modules with errors
    const file = imp_by.file;
    const tree: Ast = file.tree.?;
    for (tree.spanToList(0)) |i| {
        const node = tree.node(i);
        if (node.tag != .import) continue;
        const mod_name = tree.modName(i);
        const resolved = try imp_by.resolve(mod_name);
        defer self.allocator.free(resolved);
        if (std.mem.eql(u8, resolved, mod.path)) {
            const err = FileError{
                .src_offset = node.src_offset,
                .data = .{
                    .unresolved_module = .{
                        .errname = errname,
                        .mod_path = mod.path,
                    },
                },
            };
            const loc = file.getErrorLoc(i);
            try err.write(errwriter, errconfig, mod.file.path, mod.file.source.?, loc);
        }
    }
}

fn writeModuleAliasError(
    self: Self,
    aliaser: Aliaser,
    errwriter: anytype,
    errconfig: std.io.tty.Config,
    mod: *const Module,
) !void {
    const source = try mod.source();
    defer self.allocator.free(source);
    try aliaser.writeErrors(errwriter, errconfig, mod.file.tree.?, mod.path, source);
}

fn testBundle(comptime entry: []const u8, comptime expected: [:0]const u8) !void {
    const allocator = std.testing.allocator;

    var thread_pool: ThreadPool = undefined;
    try thread_pool.init(.{ .allocator = allocator });
    defer thread_pool.deinit();

    const opts = Options{
        .tree_shake = TreeShakeOptions{},
        .minify = false,
    };
    var bundler = try Self.init(allocator, &thread_pool, opts);
    defer bundler.deinit();

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var errbuf = std.ArrayList(u8).init(allocator);
    defer errbuf.deinit();

    try bundler.bundle(buffer.writer(), errbuf.writer(), .no_color, entry);

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
