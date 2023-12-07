const std = @import("std");
const Ast = @import("./Ast.zig");
const AstBuilder = @import("./Builder.zig");
const Node = @import("./Node.zig");
const File = @import("../file/File.zig");
const FileError = @import("../file/Error.zig");
const Loc = @import("../file/Loc.zig");
const Module = @import("../module.zig");
const Modules = @import("../bundler.zig").Modules;
const builtins = @import("../wgsl/Token.zig").builtins;

const Self = @This();
const Allocator = std.mem.Allocator;
/// Keys are indexes into builder.identifiers
const Scope = std.AutoHashMap(Node.IdentIndex, SymbolData);
const Symbols = std.ArrayHashMap(Symbol, SymbolData, Symbol.StringContext, true);
const Scopes = std.ArrayList(Scope);
const Visited = std.StringArrayHashMap(void);
const ModuleScopes = std.StringHashMap(Scope);

allocator: Allocator,
/// Outermost is module scope.
scopes: Scopes,
/// Symbols that each module exports
symbols: Symbols,
/// Global scope for each module, initialized first.
module_scopes: ModuleScopes,
/// Used to crawl modules
modules: *const Modules,
/// Won't append comments showing which module code came from.
minify: bool,

/// Accumulator since MUST be at top of WGSL file
directives: Directives,
/// The main event
builder: AstBuilder,
/// Used to build lists
scratch: std.ArrayListUnmanaged(Node.Index) = .{},
roots: std.ArrayListUnmanaged(Node.Index) = .{},

pub const Error = error{Aliasing};
pub const ErrorTag = enum {
    symbol_already_declared,
    no_matching_export,
    unresolved_ref,
};

const Directives = struct {
    const StringSet = std.StringArrayHashMap(void);
    const Diagnostics = std.AutoArrayHashMap(Node.DiagnosticControl, void);
    enables: StringSet,
    requires: StringSet,
    diagnostics: Diagnostics,

    pub fn init(allocator: Allocator) Directives {
        return Directives{
            .enables = StringSet.init(allocator),
            .requires = StringSet.init(allocator),
            .diagnostics = Diagnostics.init(allocator),
        };
    }

    pub fn deinit(self: *Directives) void {
        self.enables.deinit();
        self.requires.deinit();
        self.diagnostics.deinit();
    }
};
const Symbol = struct {
    module: []const u8,
    symbol: []const u8,

    pub const StringContext = struct {
        pub fn hash(self: @This(), s: Symbol) u32 {
            _ = self;
            var hasher = std.hash.Wyhash.init(0);
            hasher.update(s.module);
            hasher.update(s.symbol);
            return @truncate(hasher.final());
        }
        pub fn eql(self: @This(), a: Symbol, b: Symbol, b_index: usize) bool {
            _ = self;
            _ = b_index;
            return std.mem.eql(u8, a.module, b.module) and std.mem.eql(u8, a.symbol, b.symbol);
        }
    };
};
const SymbolData = struct {
    ident: Node.IdentIndex,
    src_offset: Loc.Index,
};
/// Keys are indexes into builder.identifiers
const SymbolNames = std.AutoHashMap(Node.IdentIndex, void);

pub fn init(allocator: Allocator, modules: *const Modules, minify: bool) !Self {
    var res = Self{
        .allocator = allocator,
        .symbols = Symbols.init(allocator),
        .scopes = Scopes.init(allocator),
        .modules = modules,
        .module_scopes = ModuleScopes.init(allocator),
        .minify = minify,
        .directives = Directives.init(allocator),
        .builder = try AstBuilder.init(allocator),
    };

    var symbol_names = SymbolNames.init(allocator);
    defer symbol_names.deinit();

    for (modules.values()) |m| {
        const scope = try res.moduleScope(&symbol_names, m);
        try res.module_scopes.put(m.path, scope);
    }

    return res;
}

pub fn deinit(self: *Self) void {
    for (self.scopes.items) |*s| s.deinit();
    self.scopes.deinit();
    var iter = self.module_scopes.valueIterator();
    while (iter.next()) |s| s.deinit();
    self.module_scopes.deinit();
    self.symbols.deinit();
    self.directives.deinit();
    self.builder.deinit(self.allocator);
    self.scratch.deinit(self.allocator);
    self.roots.deinit(self.allocator);
}

fn putModuleSymbol(
    self: *Self,
    symbol_names: *SymbolNames,
    scope: *Scope,
    module: []const u8,
    symbol: []const u8,
    src_offset: Loc.Index,
    alias: ?[]const u8,
) !void {
    const key = Symbol{ .module = module, .symbol = symbol };
    var symbol_gop = try self.symbols.getOrPut(key);
    if (!symbol_gop.found_existing) {
        const ident = try self.uniqueIdent(symbol_names, symbol);
        try symbol_names.putNoClobber(ident, {});
        symbol_gop.value_ptr.* = SymbolData{
            .ident = ident,
            .src_offset = src_offset,
        };
        // std.debug.print("putSymbol {s} {s} = {d} ({s})\n", .{ module, symbol, ident, self.builder.identifiers.keys()[ident - 1] });
    }

    const scope_ident = try self.builder.getOrPutIdent(self.allocator, alias orelse symbol);
    var scope_gop = try scope.getOrPut(scope_ident);
    if (scope_gop.found_existing) {
        const file = self.modules.get(module).?.file;
        try symbolAlreadyDecl(&self.builder, file, src_offset, scope_gop.value_ptr.*.src_offset);
    } else {
        scope_gop.value_ptr.* = symbol_gop.value_ptr.*;
        // std.debug.print("putScope {s}: {d}\n", .{ self.builder.identifiers.keys()[scope_ident - 1], symbol_gop.value_ptr.*.ident });
    }
}

fn moduleScope(self: *Self, symbol_names: *SymbolNames, m: Module) !Scope {
    var scope = Scope.init(self.allocator);
    const tree = m.file.tree.?;

    for (tree.spanToList(0)) |n| {
        const node = tree.node(n);
        const global_name = tree.globalName(n);
        if (global_name.len > 0) {
            try self.putModuleSymbol(
                symbol_names,
                &scope,
                m.path,
                global_name,
                node.src_offset,
                null,
            );
            continue;
        }
        if (node.tag != .import) continue;
        const imp_mod_name = tree.identifier(node.rhs);
        const resolved = try m.resolve(imp_mod_name);
        defer self.allocator.free(resolved);
        const imp_mod = self.modules.get(resolved).?;
        for (tree.spanToList(node.lhs)) |a| {
            const imp_alias = tree.node(a);
            const name = tree.identifier(imp_alias.lhs);
            const alias = if (imp_alias.rhs == 0) null else tree.identifier(imp_alias.rhs);
            try self.putModuleSymbol(
                symbol_names,
                &scope,
                imp_mod.path,
                name,
                imp_alias.src_offset,
                alias,
            );
        }
    }
    return scope;
}

fn symbolAlreadyDecl(builder: *AstBuilder, file: File, dup: Loc.Index, prev: Loc.Index) !void {
    const err_data = .{ .aliasing = .{ .tag = .symbol_already_declared } };
    // TODO: get identifier tokens
    const err1 = file.makeError(dup, dup, err_data);
    var err2 = file.makeError(prev, prev, err_data);
    err2.severity = .note;
    try builder.errors.appendSlice(file.allocator, &.{ err1, err2 });
}

fn aliasAllInner(self: *Self, visited: *Visited, module: Module) !void {
    var gop = try visited.getOrPut(module.path);
    if (gop.found_existing) return;
    for (module.import_table.keys()) |k| {
        const m = self.modules.get(k).?;
        try self.aliasAllInner(visited, m);
    }
    const tree = module.file.tree.?;
    if (!self.minify) try self.appendComment(module.path);
    for (tree.spanToList(0)) |n| {
        const module_scope = self.module_scopes.get(module.path).?;
        try self.scopes.append(module_scope);
        defer self.scopes.clearRetainingCapacity();

        const index = self.visit(module, n) catch |err| switch (err) {
            Error.Aliasing => continue,
            else => return err,
        };
        if (index != 0) try self.roots.append(self.allocator, index);
    }
}

pub fn aliasAll(self: *Self, module: []const u8) !Ast {
    // Only crawl if no errors on init
    if (self.builder.errors.items.len == 0) {
        var visited = Visited.init(self.allocator);
        defer visited.deinit();
        const mod = self.modules.get(module).?;
        try self.aliasAllInner(&visited, mod);
    }
    return try self.toOwnedAst();
}

fn appendComment(self: *Self, comment: []const u8) !void {
    const text = try self.getOrPutToken(comment);
    const node = try self.addNode(Node{ .tag = .comment, .lhs = text });
    try self.roots.append(self.allocator, node);
}

// For debugging
fn printScopes(scopes: Scopes) void {
    for (scopes.items, 0..) |s, i| {
        for (s.keys()) |k| {
            for (0..i) |_| std.debug.print("  ", .{});
            std.debug.print("{s}\n", .{k});
        }
    }
}

fn listToSpan(self: *Self, list: []const Node.Index) !Node.Index {
    return try self.builder.listToSpan(self.allocator, 0, list);
}

fn addNode(self: *Self, node: Node) !Node.Index {
    return try self.builder.addNode(self.allocator, node);
}

fn addExtra(self: *Self, extra: anytype) !Node.Index {
    return try self.builder.addExtra(self.allocator, extra);
}

fn pushScope(self: *Self) !void {
    try self.scopes.append(Scope.init(self.allocator));
}

fn popScope(self: *Self) void {
    var s = self.scopes.pop();
    s.deinit();
}

const IdentType = enum {
    /// var foo
    decl,
    /// foo + 4
    ref,
    /// requires foo
    token,
};

/// Caller owns returned slice
fn makeIdent(self: *Self, symbol: []const u8, count: usize) ![]const u8 {
    if (count == 1) return try self.allocator.dupe(u8, symbol);
    return try std.fmt.allocPrint(self.allocator, "{s}{d}", .{ symbol, count });
}

fn uniqueIdent(
    self: *Self,
    symbol_names: *SymbolNames,
    symbol: []const u8,
) !Node.IdentIndex {
    const allocator = self.allocator;
    var res: Node.IdentIndex = 0;
    var count: usize = 1;
    while (true) : (count += 1) {
        var alias = try self.makeIdent(symbol, count);
        defer allocator.free(alias);
        res = try self.builder.getOrPutIdent(allocator, alias);
        if (symbol_names.get(res) == null) break;
    }

    return res;
}

fn isUniqueIdent(self: *Self, ident: Node.IdentIndex) bool {
    for (self.scopes.items) |s| {
        if (s.get(ident) != null) return false;
    }
    return true;
}

fn uniqueScopedIdent(self: *Self, symbol: []const u8) !Node.IdentIndex {
    const allocator = self.allocator;
    var res: Node.IdentIndex = 0;
    var count: usize = 1;
    while (true) : (count += 1) {
        var alias = try self.makeIdent(symbol, count);
        defer allocator.free(alias);
        res = try self.builder.getOrPutIdent(allocator, alias);
        if (self.isUniqueIdent(res)) break;
    }

    return res;
}

fn getOrPutToken(self: *Self, symbol: []const u8) !Node.IdentIndex {
    return try self.builder.getOrPutIdent(self.allocator, symbol);
}

fn getOrPutRef(
    self: *Self,
    file: File,
    symbol: []const u8,
    src_offset: Loc.Index,
) !Node.IdentIndex {
    const ident = try self.builder.getOrPutIdent(self.allocator, symbol);
    for (0..self.scopes.items.len) |i| {
        const scope = self.scopes.items[self.scopes.items.len - i - 1];
        if (scope.get(ident)) |sym| {
            // std.debug.print("ref {s} index {d}\n", .{ symbol, sym.ident });
            return sym.ident;
        }
    }
    if (self.minify and !builtins.has(symbol)) {
        const len: Loc.Index = @intCast(symbol.len);
        const err_data = .{ .aliasing = .{ .tag = .unresolved_ref } };
        var err = file.makeError(src_offset, src_offset + len, err_data);
        err.severity = .warning;
        try self.builder.errors.append(self.allocator, err);
    }

    return ident;
}

fn getOrPutDecl(
    self: *Self,
    file: File,
    symbol: []const u8,
    src_offset: Loc.Index,
) !Node.IdentIndex {
    const scope_index = self.scopes.items.len - 1;
    // Module scope has already been initialized to properly handle import aliases.
    if (scope_index == 0) return try self.getOrPutRef(file, symbol, src_offset);

    var scope: *Scope = &self.scopes.items[scope_index];
    var ident = try self.builder.getOrPutIdent(self.allocator, symbol);
    if (scope.get(ident)) |already_decl| {
        try symbolAlreadyDecl(&self.builder, file, src_offset, already_decl.src_offset);
        return Error.Aliasing;
    }
    ident = try self.uniqueScopedIdent(symbol);
    try scope.put(ident, SymbolData{ .ident = ident, .src_offset = src_offset });

    return ident;
}

inline fn identList(self: *Self, idents: [][]const u8) !Node.Index {
    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);
    for (idents) |e| {
        const ident = try self.getOrPutToken(e);
        try self.scratch.append(self.allocator, ident);
    }
    return try self.listToSpan(self.scratch.items[scratch_top..]);
}

fn toOwnedAst(self: *Self) !Ast {
    std.debug.assert(self.scratch.items.len == 0);

    const enables = self.directives.enables.keys();
    if (enables.len > 0) {
        const idents = try self.identList(enables);
        const node = try self.addNode(Node{ .tag = .enable_directive, .lhs = idents });
        try self.scratch.append(self.allocator, node);
    }
    const requires = self.directives.requires.keys();
    if (requires.len > 0) {
        const idents = try self.identList(requires);
        const node = try self.addNode(Node{ .tag = .requires_directive, .lhs = idents });
        try self.scratch.append(self.allocator, node);
    }
    for (self.directives.diagnostics.keys()) |d| {
        const control = try self.addExtra(d);
        const node = try self.addNode(Node{ .tag = .diagnostic_directive, .lhs = control });
        try self.scratch.append(self.allocator, node);
    }

    var extra = &self.builder.extra;
    try extra.appendSlice(self.allocator, self.scratch.items);
    try extra.appendSlice(self.allocator, self.roots.items);
    self.builder.finishRootSpan(self.scratch.items.len + self.roots.items.len);

    return self.builder.toOwnedAst(self.allocator);
}

fn diagnosticControl(
    self: *Self,
    tree: Ast,
    index: Node.ExtraIndex,
) !Node.DiagnosticControl {
    var diagnostic = tree.extraData(Node.DiagnosticControl, index);
    const name = tree.identifier(diagnostic.name);
    diagnostic.name = try self.getOrPutToken(name);
    if (diagnostic.field != 0) {
        const field = tree.identifier(diagnostic.field);
        diagnostic.field = try self.getOrPutToken(field);
    }
    return diagnostic;
}

fn importAliases(
    self: *Self,
    mod: Module,
    index: Node.Index,
    imp_mod: Module,
) !Node.Index {
    const tree = mod.file.tree.?;
    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);
    for (tree.spanToList(index)) |i| {
        var node = tree.node(i);
        const identifier = tree.identifier(node.lhs);
        if (self.symbols.get(.{ .module = imp_mod.path, .symbol = identifier }) == null) {
            const err = mod.file.makeErrorAdvanced(
                node.src_offset,
                .ident,
                .{ .aliasing = .{ .tag = .no_matching_export } },
            );
            try self.builder.errors.append(self.allocator, err);
            return Error.Aliasing;
        }
        node.lhs = try self.getOrPutToken(identifier);
        if (node.rhs != 0) {
            const alias = tree.identifier(node.rhs);
            node.rhs = try self.getOrPutToken(alias);
        }
        const new_node = try self.addNode(node);
        try self.scratch.append(self.allocator, new_node);
    }

    return try self.listToSpan(self.scratch.items[scratch_top..]);
}

fn visitScopedDecl(self: *Self, mod: Module, index: Node.Index) !void {
    const file = mod.file;
    const tree = file.tree.?;
    if (index == 0) return;
    var node = tree.node(index);

    switch (node.tag) {
        .@"const", .let => {
            var typed_ident = tree.extraData(Node.TypedIdent, node.lhs);
            const symbol = tree.identifier(typed_ident.name);
            _ = try self.getOrPutDecl(file, symbol, node.src_offset);
        },
        else => {},
    }
}

fn visitScopedDecls(self: *Self, mod: Module, index: Node.Index) !void {
    for (mod.file.tree.?.spanToList(index)) |i| try self.visitScopedDecl(mod, i);
}

fn visitCompound(self: *Self, mod: Module, index: Node.Index, comptime add_scope: bool) !Node.Index {
    const file = mod.file;
    const tree = file.tree.?;
    if (index == 0) return 0;
    var node = tree.node(index);

    if (add_scope) try self.pushScope();
    defer if (add_scope) self.popScope();
    node.lhs = try self.visit(mod, node.lhs);
    try self.visitScopedDecls(mod, node.rhs);
    node.rhs = try self.visit(mod, node.rhs);
    return try self.addNode(node);
}

fn visit(self: *Self, mod: Module, index: Node.Index) (Error || Allocator.Error)!Node.Index {
    const file = mod.file;
    const tree = file.tree.?;
    if (index == 0) return 0;
    var node = tree.node(index);

    switch (node.tag) {
        .comment => {},
        .span => {
            const scratch_top = self.scratch.items.len;
            defer self.scratch.shrinkRetainingCapacity(scratch_top);
            for (tree.spanToList(index)) |i| {
                try self.scratch.append(self.allocator, try self.visit(mod, i));
            }

            return try self.listToSpan(self.scratch.items[scratch_top..]);
        },
        .enable_directive, .requires_directive => |t| {
            for (tree.spanToList(node.lhs)) |i| {
                const name = tree.identifier(i);
                switch (t) {
                    .enable_directive => try self.directives.enables.put(name, {}),
                    .requires_directive => try self.directives.requires.put(name, {}),
                    else => unreachable,
                }
            }
            return 0;
        },
        .diagnostic_directive => {
            const diagnostic = try self.diagnosticControl(tree, node.lhs);
            try self.directives.diagnostics.put(diagnostic, {});
            return 0;
        },
        .global_var => {
            var global_var = tree.extraData(Node.GlobalVar, node.lhs);
            global_var.name = try self.getOrPutDecl(file, tree.identifier(global_var.name), node.src_offset);
            global_var.attrs = try self.visit(mod, global_var.attrs);
            global_var.type = try self.visit(mod, global_var.type);
            node.lhs = try self.addExtra(global_var);
            node.rhs = try self.visit(mod, node.rhs);
        },
        .override => {
            var override = tree.extraData(Node.Override, node.lhs);
            override.name = try self.getOrPutDecl(file, tree.identifier(override.name), node.src_offset);
            override.attrs = try self.visit(mod, override.attrs);
            override.type = try self.visit(mod, override.type);
            node.lhs = try self.addExtra(override);
            node.rhs = try self.visit(mod, node.rhs);
        },
        .@"fn" => {
            var header = tree.extraData(Node.FnHeader, node.lhs);
            header.name = try self.getOrPutDecl(file, tree.identifier(header.name), node.src_offset);
            header.attrs = try self.visit(mod, header.attrs);
            try self.pushScope();
            defer self.popScope();
            header.params = try self.visit(mod, header.params);
            header.return_attrs = try self.visit(mod, header.return_attrs);
            header.return_type = try self.visit(mod, header.return_type);
            node.lhs = try self.addExtra(header);
            node.rhs = try self.visitCompound(mod, node.rhs, false);
        },
        .fn_param => {
            var param = tree.extraData(Node.FnParam, node.rhs);
            param.name = try self.getOrPutDecl(file, tree.identifier(param.name), node.src_offset);
            param.type = try self.visit(mod, param.type);
            node.lhs = try self.visit(mod, node.lhs);
            node.rhs = try self.addExtra(param);
        },
        .@"var" => {
            var extra = tree.extraData(Node.Var, node.lhs);
            extra.name = try self.getOrPutDecl(file, tree.identifier(extra.name), node.src_offset);
            extra.type = try self.visit(mod, extra.type);
            node.lhs = try self.addExtra(extra);
            node.rhs = try self.visit(mod, node.rhs);
        },
        .@"for" => {
            var header = tree.extraData(Node.ForHeader, node.lhs);
            header.attrs = try self.visit(mod, header.attrs);
            try self.pushScope();
            defer self.popScope();
            header.init = try self.visit(mod, header.init);
            header.cond = try self.visit(mod, header.cond);
            header.update = try self.visit(mod, header.update);
            node.lhs = try self.addExtra(header);
            node.rhs = try self.visitCompound(mod, node.rhs, false);
        },
        .@"const", .let => {
            var typed_ident = tree.extraData(Node.TypedIdent, node.lhs);
            // these have already been visited by `visitScopedDecls`
            typed_ident.name = try self.getOrPutRef(file, tree.identifier(typed_ident.name), node.src_offset);
            typed_ident.type = try self.visit(mod, typed_ident.type);
            node.lhs = try self.addExtra(typed_ident);
            node.rhs = try self.visit(mod, node.rhs);
        },
        .type_alias => {
            node.lhs = try self.getOrPutDecl(file, tree.identifier(node.lhs), node.src_offset);
            node.rhs = try self.visit(mod, node.rhs);
        },
        .type, .ident => {
            node.lhs = try self.getOrPutRef(file, tree.identifier(node.lhs), node.src_offset);
            node.rhs = try self.visit(mod, node.rhs);
        },
        .@"struct" => {
            node.lhs = try self.getOrPutDecl(file, tree.identifier(node.lhs), node.src_offset);
            node.rhs = try self.visit(mod, node.rhs);
        },
        .import => {
            const mod_name = tree.identifier(node.rhs);
            const resolved = try mod.resolve(mod_name);
            defer self.allocator.free(resolved);
            const imp_mod = self.modules.get(resolved).?;
            node.lhs = try self.importAliases(mod, node.lhs, imp_mod);
            node.rhs = try self.getOrPutToken(mod_name);
        },
        .import_alias => unreachable,
        .field_access => {
            node.lhs = try self.visit(mod, node.lhs);
            node.rhs = try self.getOrPutToken(tree.identifier(node.rhs));
        },
        .number => {
            node.lhs = try self.getOrPutToken(tree.identifier(node.lhs));
        },
        .attribute => {
            const attr: Node.Attribute = @enumFromInt(node.lhs);
            switch (attr) {
                .@"const",
                .compute,
                .fragment,
                .invariant,
                .must_use,
                .vertex,
                .builtin,
                .interpolate,
                => {},
                .diagnostic => {
                    const diagnostic = try self.diagnosticControl(tree, node.rhs);
                    node.rhs = try self.addExtra(diagnostic);
                },
                .@"align",
                .binding,
                .group,
                .id,
                .location,
                .size,
                => {
                    node.rhs = try self.visit(mod, node.rhs);
                },
                .workgroup_size => {
                    var workgroup_size = tree.extraData(Node.WorkgroupSize, node.rhs);
                    workgroup_size.x = try self.visit(mod, workgroup_size.x);
                    workgroup_size.y = try self.visit(mod, workgroup_size.y);
                    workgroup_size.z = try self.visit(mod, workgroup_size.z);
                    node.rhs = try self.addExtra(workgroup_size);
                },
            }
        },
        .compound => {
            return try self.visitCompound(mod, index, true);
        },
        .struct_member => {
            node.lhs = try self.visit(mod, node.lhs);
            var typed_ident = tree.extraData(Node.TypedIdent, node.rhs);
            typed_ident.name = try self.getOrPutToken(tree.identifier(typed_ident.name));
            typed_ident.type = try self.visit(mod, typed_ident.type);
            node.rhs = try self.addExtra(typed_ident);
        },
        .index_access,
        .const_assert,
        .increment,
        .decrement,
        .phony_assign,
        .paren,
        .logical_not,
        .bitwise_complement,
        .negative,
        .deref,
        .@"return",
        .break_if,
        .continuing,
        .ref,
        .loop,
        .@"if",
        .else_if,
        .@"else",
        .@"switch",
        .switch_body,
        .case_clause,
        .case_selector,
        .@"while",
        .call,
        .@"=",
        .@"+=",
        .@"-=",
        .@"*=",
        .@"/=",
        .@"%=",
        .@"&=",
        .@"|=",
        .@"^=",
        .@"<<=",
        .@">>=",
        .lshift,
        .rshift,
        .lt,
        .gt,
        .lte,
        .gte,
        .eq,
        .neq,
        .mul,
        .div,
        .mod,
        .add,
        .sub,
        .logical_and,
        .logical_or,
        .bitwise_and,
        .bitwise_or,
        .bitwise_xor,
        => {
            node.lhs = try self.visit(mod, node.lhs);
            node.rhs = try self.visit(mod, node.rhs);
        },
        .true, .false, .@"break", .@"continue", .discard => {},
    }

    return try self.addNode(node);
}
