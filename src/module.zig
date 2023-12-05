const std = @import("std");
const Ast = @import("./ast/Ast.zig");
const Node = @import("./ast/Node.zig").Node;
const TreeShaker = @import("./ast/TreeShaker.zig");
const Parser = @import("./wgsl/Parser.zig");
const File = @import("./file/File.zig");

pub const Error = error{UnsupportedLanguage};
pub const resolveFrom = File.resolveFrom;
const Allocator = std.mem.Allocator;
const Self = @This();
const StringSet = std.StringArrayHashMapUnmanaged(void);
const ImportTable = std.StringArrayHashMapUnmanaged(StringSet);

allocator: Allocator,
path: []const u8,
imported_by: []const u8,
file: File,
import_table: ImportTable = .{},

pub fn init(allocator: Allocator, path: []const u8, imported_by: []const u8) Self {
    return Self{
        .allocator = allocator,
        .path = path,
        .imported_by = imported_by,
        .file = File{
            .allocator = allocator,
            .path = path,
        },
    };
}

pub fn load(self: *Self, tree_shake: ?TreeShaker.Options) !void {
    try self.file.load();

    // Get imports eagerly, even with errors
    var tree: *Ast = &self.file.tree.?;
    self.import_table = try importTable(self.allocator, self.file.path, tree.*);

    if (tree.errors.len > 0) return error.Parsing;
    if (tree_shake) |opts| try TreeShaker.treeShake(self.allocator, tree, opts);
}

pub fn deinit(self: *Self) void {
    const allocator = self.allocator;
    var iter = self.import_table.iterator();
    while (iter.next()) |kv| {
        allocator.free(kv.key_ptr.*);
        kv.value_ptr.*.deinit(allocator);
    }
    self.import_table.deinit(allocator);
    self.file.deinit();
}

pub fn hasError(self: Self) bool {
    return self.file.tree == null or self.file.tree.?.errors.len > 0;
}

pub fn writeParsingErrors(self: Self, writer: anytype, config: std.io.tty.Config) !void {
    const t = self.file.tree.?;
    for (t.errors) |e| try e.write(writer, config);
}

/// Caller owns returned slice
pub fn resolve(self: Self, relpath: []const u8) ![]const u8 {
    return try File.resolveFrom(self.allocator, self.path, relpath);
}

fn importTable(allocator: Allocator, path: []const u8, tree: Ast) !ImportTable {
    var res = ImportTable{};

    for (tree.spanToList(0)) |i| {
        if (tree.node(i).tag != .import) continue;
        const mod_name = tree.modName(i);
        const resolved = try resolveFrom(allocator, path, mod_name);
        const mod_imports = try res.getOrPut(allocator, resolved);
        if (!mod_imports.found_existing) mod_imports.value_ptr.* = StringSet{};

        for (tree.modAliases(i)) |j| {
            const mod_symbol = tree.identifier(tree.node(j).lhs);
            try mod_imports.value_ptr.*.put(allocator, mod_symbol, {});
        }
    }

    return res;
}
