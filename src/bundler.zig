const std = @import("std");
const Module = @import("module.zig");
const Renderer = @import("renderer.zig").Renderer;

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

/// Caller owns returned slice.
pub fn bundle(self: *Self, writer: anytype, entry: []const u8) !void {
    const resolved = try std.fs.path.resolve(self.allocator, &[_][]const u8{ ".", entry });
    defer self.allocator.free(resolved);
    try self.modules.put(resolved, Module{ .allocator = self.allocator, .name = resolved });

    // These jobs are to tokenize, parse and scan files for imports
    var wait_group: WaitGroup = .{};
    wait_group.reset();
    wait_group.start();
    try self.thread_pool.spawn(workerAst, .{
        self,
        &wait_group,
        resolved,
    });
    wait_group.wait();

    var renderer = Renderer(@TypeOf(writer)){ .underlying_writer = writer };
    var visited = Visited.init(self.allocator);
    defer visited.deinit();
    try self.render(&renderer, &visited, self.modules.get(resolved).?);
}

fn workerAst(self: *Self, wait_group: *WaitGroup, path: []const u8) void {
    var mod = self.modules.getPtr(path).?;
    defer wait_group.finish();

    switch (mod.status) {
        .empty => {
            mod.read() catch return;
            mod.parse() catch return;
        },
        .read => {
            mod.parse() catch return;
        },
        .parsed => return,
    }
    if (mod.status != .parsed) return;

    self.mutex.lock();
    for (mod.import_table.keys()) |k| {
        const gop = self.modules.getOrPut(k) catch return;
        if (!gop.found_existing) {
            gop.value_ptr.* = Module{ .allocator = self.allocator, .name = k };
            wait_group.start();
            self.thread_pool.spawn(workerAst, .{ self, wait_group, k }) catch {
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
