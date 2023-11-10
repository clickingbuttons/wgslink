const std = @import("std");
const Module = @import("module.zig");
const Renderer = @import("renderer.zig").Renderer;
const TreeShakeOptions = @import("./tree_shaker.zig").Options;

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
mutex: Mutex = .{},
/// Cache
modules: Modules,

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

pub fn render(self: Self, renderer: anytype, visited: *Visited, module: Module) !void {
    if ((try visited.getOrPut(module.name)).found_existing) return;
    std.debug.print("render {s}\n", .{module.name});
    for (module.import_table.keys()) |k| {
        if (self.modules.get(k)) |m| try self.render(renderer, visited, m);
    }
    if (module.tree) |t| {
        try renderer.print("// {s}\n", .{module.name});
        try renderer.writeTranslationUnit(t);
    }
}

pub const Options = struct {
    file: []const u8,
    tree_shake: ?TreeShakeOptions,
};

/// Caller owns returned slice.
pub fn bundle(self: *Self, writer: anytype, opts: Options) !void {
    const resolved = try std.fs.path.resolve(self.allocator, &[_][]const u8{ ".", opts.file });
    defer self.allocator.free(resolved);
    try self.modules.put(resolved, Module{ .allocator = self.allocator, .name = resolved });

    // These jobs are to tokenize, parse and scan files for imports
    var wait_group: WaitGroup = .{};
    wait_group.reset();
    wait_group.start();
    try self.thread_pool.spawn(workerAst, .{ self, &wait_group, resolved, opts.tree_shake });
    wait_group.wait();

    var renderer = Renderer(@TypeOf(writer)){ .underlying_writer = writer };
    var visited = Visited.init(self.allocator);
    defer visited.deinit();
    try self.render(&renderer, &visited, self.modules.get(resolved).?);
}

fn workerAst(self: *Self, wait_group: *WaitGroup, path: []const u8, tree_shake: ?TreeShakeOptions) void {
    var mod = self.modules.getPtr(path).?;
    defer wait_group.finish();

    switch (mod.status) {
        .empty => {
            mod.read() catch return;
            mod.parse(tree_shake) catch return;
        },
        .read => {
            mod.parse(tree_shake) catch return;
        },
        .parsed => return,
    }
    if (mod.status != .parsed) return;

    self.mutex.lock();
    var iter = mod.import_table.iterator();
    while (iter.next()) |kv| {
        const gop = self.modules.getOrPut(kv.key_ptr.*) catch return;
        if (!gop.found_existing) {
            gop.value_ptr.* = Module{ .allocator = self.allocator, .name = kv.key_ptr.* };
            wait_group.start();
            var next_tree_shake = tree_shake;
            if (next_tree_shake) |*t| t.symbols =  kv.value_ptr.*.keys();
            self.thread_pool.spawn(workerAst, .{ self, wait_group, kv.key_ptr.*, next_tree_shake }) catch {
                wait_group.finish();
                continue;
            };
        }
    }
    self.mutex.unlock();
}

test "bundler" {
    const allocator = std.testing.allocator;

    var thread_pool: ThreadPool = undefined;
    try thread_pool.init(.{ .allocator = allocator });
    defer thread_pool.deinit();

    var bundler = try Self.init(allocator, &thread_pool);
    defer bundler.deinit();

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    const path = "./src/wgsl/test/cube-map.wgsl";
    try bundler.bundle(buffer.writer(), path);

    var source_file = try std.fs.cwd().openFile(path, .{});
    defer source_file.close();
    const source = try source_file.readToEndAllocOptions(allocator, std.math.maxInt(u32), null, 1, 0);
    defer allocator.free(source);

    try std.testing.expectEqualStrings(source, buffer.items);
}
