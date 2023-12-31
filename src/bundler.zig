const std = @import("std");
const Module = @import("module.zig");
const TreeShaker = @import("./ast/TreeShaker.zig");
const Ast = @import("./ast/Ast.zig");
const Linker = @import("./ast/Linker.zig");
const FileError = @import("./file/Error.zig");
const renderer = @import("./WgslRenderer.zig").renderer;
const fail = @import("./main.zig").fail;

const Self = @This();
const Allocator = std.mem.Allocator;
const ThreadPool = std.Thread.Pool;
const Mutex = std.Thread.Mutex;
const WaitGroup = std.Thread.WaitGroup;
pub const Modules = std.StringArrayHashMap(Module);
pub const Options = struct {
    tree_shake: bool = true,
    /// If len = 0 will try to find entrypoints
    entrypoints: []const []const u8 = &.{},
    minify: bool = false,
};
const ErrConfig = std.io.tty.Config;

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

/// Caller owns returned slice but not its elements.
fn getEntrypoints(self: *Self, root_tree: Ast) ![]const []const u8 {
    if (self.opts.entrypoints.len > 0) return try self.allocator.dupe([]const u8, self.opts.entrypoints);
    var found = try TreeShaker.entrypoints(self.allocator, root_tree);
    defer found.deinit();

    var res = std.ArrayList([]const u8).init(self.allocator);
    defer res.deinit();
    for (found.keys()) |k| try res.append(root_tree.identifier(k));
    return try res.toOwnedSlice();
}

/// Caller owns returned tree allocated with self.allocator.
pub fn bundle(self: *Self, errwriter: anytype, errconfig: ErrConfig, fname: []const u8) !Ast {
    const resolved = try Module.resolveFrom(self.allocator, "./a", fname);
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

    const entrypoints = try self.getEntrypoints(mod.*.file.tree.?);
    defer self.allocator.free(entrypoints);

    var linker = try Linker.init(self.allocator, &self.modules, self.opts.minify, entrypoints);
    defer linker.deinit();
    var tree = try linker.link(mod.path);
    errdefer tree.deinit(self.allocator);
    if (tree.errors.len > 0) {
        var fatal = false;
        for (tree.errors) |e| {
            if (e.severity == .@"error") fatal = true;
            try e.write(errwriter, errconfig);
        }
        if (fatal) return error.Aliasing;
    }

    if (self.opts.tree_shake) try TreeShaker.treeShake(self.allocator, &tree, entrypoints);
    return tree;
}

/// tokenize, parse and scan files for imports
fn workerAst(
    self: *Self,
    errwriter: anytype,
    errconfig: ErrConfig,
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
    errconfig: ErrConfig,
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
                .string,
                .{ .unresolved_module = .{ .errname = errname, .mod_path = mod.path } },
            );
            try err.write(errwriter, errconfig);
        }
    }
}

fn testBundle(comptime entry: []const u8, comptime expected: [:0]const u8) !void {
    const allocator = std.testing.allocator;

    var thread_pool: ThreadPool = undefined;
    try thread_pool.init(.{ .allocator = allocator });
    defer thread_pool.deinit();

    var bundler = try Self.init(allocator, &thread_pool, .{});
    defer bundler.deinit();

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    const stderr = std.io.getStdErr();
    const errconfig = std.io.tty.detectConfig(stderr);
    var tree = try bundler.bundle(stderr.writer(), errconfig, entry);
    defer tree.deinit(allocator);

    var wgsl = renderer(buffer.writer(), .{});
    try wgsl.writeTranslationUnit(tree);

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
        \\@vertex fn main() -> @builtin(position) vec4f {
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
        \\@vertex fn main() -> @builtin(position) vec4f {
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
        \\@vertex fn main() -> @builtin(position) vec4f {
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
        \\@vertex fn main() -> @builtin(position) vec4f {
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
        \\@vertex fn main() -> @builtin(position) vec4f {
        \\  return vec4f(a);
        \\}
    );
}

test "cycle ident clash bundle" {
    try testBundle("./test/bundle-cycle-ident-clash/a.wgsl",
        \\// test/bundle-cycle-ident-clash/c.wgsl
        \\const c = 3.0;
        \\// test/bundle-cycle-ident-clash/b.wgsl
        \\const b = 2.0 + c + a2;
        \\const a2 = 1.0;
        \\// test/bundle-cycle-ident-clash/a.wgsl
        \\const a = 1.0 + b;
        \\@vertex fn main() -> @builtin(position) vec4f {
        \\  const b3 = 2.0;
        \\  return vec4f(a + b3);
        \\}
    );
}
