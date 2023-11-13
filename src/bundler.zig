const std = @import("std");
const Module = @import("module.zig");
const Renderer = @import("renderer.zig").Renderer;
const TreeShakeOptions = @import("./tree_shaker.zig").Options;
const Ast = @import("./wgsl/Ast.zig");

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

pub fn render(self: Self, renderer: anytype, visited: *Visited, module: *const Module) !void {
    if ((try visited.getOrPut(module.name)).found_existing) return;
    if (module.file) |f| {
        for (f.import_table.keys()) |k| {
            const m = self.modules.getPtr(k).?;
            try self.render(renderer, visited, m);
        }
        try module.render(renderer);
    }
}

pub const Options = struct {
    file: []const u8,
    tree_shake: ?TreeShakeOptions,
};

pub fn bundle(
    self: *Self,
    writer: anytype,
    errwriter: anytype,
    errconfig: std.io.tty.Config,
    opts: Options,
) !void {
    const resolved = try Module.resolve(self.allocator, "./foo.wgsl", opts.file);
    defer self.allocator.free(resolved);
    try self.modules.put(resolved, Module{
        .allocator = self.allocator,
        .name = resolved,
        .imported_by = "bundle api",
    });
    const mod = self.modules.getPtr(resolved).?;

    // These jobs are to tokenize, parse and scan files for imports
    var wait_group: WaitGroup = .{};
    wait_group.start();
    try self.thread_pool.spawn(workerAst, .{ self, errwriter, errconfig, &wait_group, mod, opts.tree_shake });
    wait_group.wait();
    for (self.modules.values()) |v| if (!v.parsed) return error.UnparsedModule;

    var renderer = Renderer(@TypeOf(writer)){ .underlying_writer = writer };
    var visited = Visited.init(self.allocator);
    defer visited.deinit();
    try self.render(&renderer, &visited, mod);
}

fn workerAst(
    self: *Self,
    errwriter: anytype,
    errconfig: std.io.tty.Config,
    wait_group: *WaitGroup,
    mod: *Module,
    tree_shake: ?TreeShakeOptions,
) void {
    defer wait_group.finish();

    mod.init(tree_shake) catch |err| {
        self.stderr_mutex.lock();
        defer self.stderr_mutex.unlock();
        switch (err) {
            error.Parsing => {
                if (mod.file) |f| {
                    for (f.tree.errors) |e| f.tree.renderError(e, errwriter, errconfig, mod.name) catch {};
                }
            },
            else => |t| {
                errwriter.print("error: {s} for {s} (imported by {s})\n", .{ @errorName(t), mod.name, mod.imported_by }) catch {};
                if (self.modules.get(mod.imported_by)) |imp| {
                    const tree: Ast = imp.file.?.tree;
                    for (tree.spanToList(0)) |node| {
                        if (tree.nodeTag(node) != .import) continue;
                        const mod_name = tree.moduleName(node);
                        const resolved = Module.resolve(self.allocator, imp.name, mod_name) catch continue;
                        defer self.allocator.free(resolved);
                        if (std.mem.eql(u8, resolved, mod.name)) {
                            tree.renderError(Ast.Error{
                                .tag = .unresolved_module,
                                .token = tree.nodeRHS(node),
                            }, errwriter, errconfig, mod.imported_by) catch {};
                        }
                    }
                } else {
                    errwriter.print("sad\n", .{}) catch {};
                }
            },
        }
        return;
    };

    self.modules_mutex.lock();
    defer self.modules_mutex.unlock();
    var iter = mod.file.?.import_table.iterator();
    while (iter.next()) |kv| {
        const gop = self.modules.getOrPut(kv.key_ptr.*) catch return;
        if (!gop.found_existing) {
            gop.value_ptr.* = Module{
                .allocator = self.allocator,
                .name = kv.key_ptr.*,
                .imported_by = mod.name,
            };
            wait_group.start();
            self.thread_pool.spawn(workerAst, .{
                self, errwriter, errconfig, wait_group, gop.value_ptr, TreeShakeOptions{
                    .symbols = kv.value_ptr.*.keys(),
                    .find_symbols = false,
                },
            }) catch {
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
    });

    try std.testing.expectEqualStrings(expected ++ "\n", buffer.items);
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
