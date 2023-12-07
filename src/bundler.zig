const std = @import("std");
const Module = @import("module.zig");
const Renderer = @import("./WgslRenderer.zig").Renderer;
const TreeShaker = @import("./ast/TreeShaker.zig");
const Ast = @import("./ast/Ast.zig");
const Aliaser = @import("./ast/Aliaser.zig");
const FileError = @import("./file/Error.zig");
const node_mod = @import("./ast/Node.zig");

const Self = @This();
const Allocator = std.mem.Allocator;
const ThreadPool = std.Thread.Pool;
const Mutex = std.Thread.Mutex;
const WaitGroup = std.Thread.WaitGroup;
pub const Modules = std.StringArrayHashMap(Module);
pub const Options = struct {
    entrypoints: ?[]const []const u8,
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

fn treeShake(self: *Self, root_tree: Ast, tree: *Ast) !void {
    var entrypoints = self.opts.entrypoints orelse return;
    if (entrypoints.len == 0) {
        var found = try TreeShaker.entrypoints(self.allocator, root_tree);
        defer found.deinit();

        var all_symbols = std.ArrayList([]const u8).init(self.allocator);
        defer all_symbols.deinit();
        for (found.keys()) |k| try all_symbols.append(root_tree.identifier(k));
        const owned = try all_symbols.toOwnedSlice();
        defer self.allocator.free(owned);

        try TreeShaker.treeShake(self.allocator, tree, owned);
    } else {
        try TreeShaker.treeShake(self.allocator, tree, entrypoints);
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
    });
    wait_group.wait();
    var has_unparsed = false;
    for (self.modules.values()) |v| {
        if (v.hasError()) has_unparsed = true;
    }
    if (has_unparsed) return error.UnparsedModule;

    var aliaser = try Aliaser.init(self.allocator, &self.modules, self.opts.minify);
    defer aliaser.deinit();
    var tree = try aliaser.aliasAll(mod.path);
    defer tree.deinit(self.allocator);
    if (tree.errors.len > 0) {
        var fatal = false;
        for (tree.errors) |e| {
            if (e.severity == .@"error") fatal = true;
            try e.write(errwriter, errconfig);
        }
        if (fatal) return error.Aliasing;
    }

    try self.treeShake(mod.file.tree.?, &tree);
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
) void {
    defer wait_group.finish();

    mod.load() catch |err| {
        self.stderr_mutex.lock();
        defer self.stderr_mutex.unlock();
        switch (err) {
            error.Parsing => mod.writeParsingErrors(errwriter, errconfig) catch {},
            // Probably some kind of file opening error...
            else => |t| self.writeFileOpenError(errwriter, errconfig, mod, @errorName(t)) catch {},
        }
        return;
    };

    self.modules_mutex.lock();
    defer self.modules_mutex.unlock();
    var iter = mod.import_table.iterator();
    while (iter.next()) |kv| {
        const mod_name = kv.key_ptr.*;
        const gop = self.modules.getOrPut(mod_name) catch return;
        if (!gop.found_existing) {
            gop.value_ptr.* = Module.init(self.allocator, mod_name, mod.path);
            wait_group.start();
            self.thread_pool.spawn(workerAst, .{
                self,
                errwriter,
                errconfig,
                wait_group,
                gop.value_ptr,
            }) catch {
                wait_group.finish();
                continue;
            };
        }
    }
}

fn writeFileOpenError(
    self: Self,
    errwriter: anytype,
    errconfig: std.io.tty.Config,
    mod: *Module,
    errname: []const u8,
) !void {
    const imp_by: Module = self.modules.get(mod.imported_by) orelse {
        try errwriter.print("{s} opening file {s}\n", .{ errname, mod.path });
        return;
    };
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
            const err = file.makeErrorAdvanced(
                node.src_offset,
                .string_literal,
                .{ .unresolved_module = .{ .errname = errname, .mod_path = mod.path } },
            );
            try err.write(errwriter, errconfig);
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
        .tree_shake = TreeShaker.Options{},
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
        \\var a2 = 2.0;
        \\var b = a2 + 3.0;
        \\// test/bundle-ident-clash/a.wgsl
        \\var a = 1.0 + b;
        \\@vertex fn main() -> @location(0) vec4f {
        \\  return vec4f(a);
        \\}
    );
}
