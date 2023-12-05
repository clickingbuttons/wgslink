const std = @import("std");
const Ast = @import("./Ast.zig");
const AstBuilder = @import("./Builder.zig");
const Node = @import("./Node.zig");
const File = @import("../file/File.zig");
const FileError = @import("../file/Error.zig");
const Loc = @import("../file/Loc.zig");
const Module = @import("../module.zig");

const Self = @This();
const Allocator = std.mem.Allocator;
const Symbol = struct {
    ident: Node.IdentIndex,
    src_offset: Loc.Index,
};
const Scope = std.StringArrayHashMap(Symbol);
const Scopes = std.ArrayList(Scope);

allocator: Allocator,
scopes: Scopes,
// Used to check for redeclared symbols
module_scope: Scope,
/// Used to build errors
module: ?*const Module = null,
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

pub fn init(allocator: Allocator, minify: bool) !Self {
    var scopes = Scopes.init(allocator);
    // Global scope
    try scopes.append(Scope.init(allocator));
    var res = Self{
        .allocator = allocator,
        .scopes = scopes,
        .module_scope = Scope.init(allocator),
        .minify = minify,
        .directives = Directives.init(allocator),
        .builder = try AstBuilder.init(allocator),
    };
    return res;
}

pub fn deinit(self: *Self) void {
    for (self.scopes.items) |*s| {
        for (s.keys()) |k| self.allocator.free(k);
        s.deinit();
    }
    self.scopes.deinit();
    self.module_scope.deinit();
    self.directives.deinit();
    self.builder.deinit(self.allocator);
    self.scratch.deinit(self.allocator);
    self.roots.deinit(self.allocator);
}

/// Add module to intermediate data structures. Caller should check `builder.errors` after.
pub fn append(self: *Self, module: *const Module) !void {
    self.module = module;
    const tree = module.file.tree.?;
    if (!self.minify) try self.appendComment(module.path);
    self.module_scope.clearRetainingCapacity();
    for (tree.spanToList(0)) |n| {
        const index = self.visit(tree, n) catch |err| switch (err) {
            Error.Aliasing => continue,
            else => return err,
        };
        if (index != 0) try self.roots.append(self.allocator, index);
    }
}

fn appendComment(self: *Self, comment: []const u8) !void {
    const text = try self.getOrPutIdent(.token, comment, 0);
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
    for (s.keys()) |k| self.allocator.free(k);
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

fn isUniqueIdent(self: *Self, ident: []const u8) bool {
    for (self.scopes.items) |s| {
        if (s.get(ident) != null) return false;
    }

    return true;
}

/// Caller owns returned slice
fn makeUniqueIdent(self: *Self, ident: []const u8) ![]const u8 {
    var count: usize = 2;
    var new_ident = try std.fmt.allocPrint(self.allocator, "{s}{d}", .{ ident, count });
    while (!self.isUniqueIdent(new_ident)) {
        count += 1;
        self.allocator.free(new_ident);
        new_ident = try std.fmt.allocPrint(self.allocator, "{s}{d}", .{ ident, count });
    }

    return new_ident;
}

fn getOrPutModuleDecl(
    self: *Self,
    ident: []const u8,
    src_offset: Loc.Index,
) !void {
    var gop = try self.module_scope.getOrPut(ident);
    if (gop.found_existing) {
        // printScopes(self.scopes);
        const file = self.module.?.file;

        try self.builder.errors.append(
            self.allocator,
            file.makeError(
                src_offset,
                src_offset, // TODO: get identifier token
                .{ .aliasing = .{ .tag = .symbol_already_declared } },
            ),
        );
        var err = file.makeError(
            gop.value_ptr.*.src_offset,
            gop.value_ptr.*.src_offset, // TODO: get identifier token
            .{ .aliasing = .{ .tag = .symbol_already_declared } },
        );
        err.severity = .note;
        try self.builder.errors.append(self.allocator, err);
        return Error.Aliasing;
    } else {
        gop.value_ptr.* = .{
            .ident = try self.builder.getOrPutIdent(self.allocator, ident),
            .src_offset = src_offset,
        };
    }
}

// const err = self.module.?.file.makeErrorAdvanced(
//     src_offset,
//     .ident,
//     .{ .aliasing = .{ .tag = .no_matching_export } },
// );
// try self.builder.errors.append(self.allocator, err);
// return Error.Aliasing;

fn getOrPutIdent(
    self: *Self,
    comptime ty: IdentType,
    ident: []const u8,
    src_offset: Loc.Index,
) !Node.IdentIndex {
    switch (ty) {
        .decl => {
            std.debug.print("getOrPutIdent {s} {s}\n", .{ @tagName(ty), ident });
            try self.getOrPutModuleDecl(ident, src_offset);
            const scope_index = self.scopes.items.len - 1;
            var scope: *Scope = &self.scopes.items[scope_index];

            const clashes = scope.get(ident) != null;
            const alias = if (clashes)try self.makeUniqueIdent(ident) else try self.allocator.dupe(u8, ident);

            const symbol = Symbol{
                .ident = try self.builder.getOrPutIdent(self.allocator, alias),
                .src_offset = src_offset,
            };
            try scope.put(alias, symbol);
            if (clashes) {
                const gop = try scope.getOrPut(ident);
                std.debug.assert(gop.found_existing);
                gop.value_ptr.* = symbol;
            }
            return symbol.ident;
        },
        .ref => {
            for (0..self.scopes.items.len) |i| {
                const s = self.scopes.items[self.scopes.items.len - i - 1];
                if (s.get(ident)) |sym| {
                    // std.debug.print("ref {s} index {d}\n", .{ ident, sym.ident });
                    return sym.ident;
                }
            }
            return try self.builder.getOrPutIdent(self.allocator, ident);
        },
        .token => {
            return try self.builder.getOrPutIdent(self.allocator, ident);
        },
    }
}

inline fn identList(self: *Self, idents: [][]const u8) !Node.Index {
    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);
    for (idents) |e| {
        const ident = try self.getOrPutIdent(.token, e, 0);
        try self.scratch.append(self.allocator, ident);
    }
    return try self.listToSpan(self.scratch.items[scratch_top..]);
}

inline fn typedIdent(
    self: *Self,
    comptime ty: IdentType,
    tree: Ast,
    index: Node.Index,
    src_offset: Loc.Index,
) !Node.Index {
    var typed_ident = tree.extraData(Node.TypedIdent, index);
    typed_ident.name = try self.getOrPutIdent(ty, tree.identifier(typed_ident.name), src_offset);
    typed_ident.type = try self.visit(tree, typed_ident.type);
    return try self.addExtra(typed_ident);
}

pub fn toOwnedAst(self: *Self, lang: File.Language) !Ast {
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

    return self.builder.toOwnedAst(self.allocator, lang);
}

fn diagnosticControl(
    self: *Self,
    tree: Ast,
    index: Node.ExtraIndex,
) !Node.DiagnosticControl {
    var diagnostic = tree.extraData(Node.DiagnosticControl, index);
    const name = tree.identifier(diagnostic.name);
    diagnostic.name = try self.getOrPutIdent(.token, name, 0);
    if (diagnostic.field != 0) {
        const field = tree.identifier(diagnostic.field);
        diagnostic.field = try self.getOrPutIdent(.token, field, 0);
    }
    return diagnostic;
}

fn visit(self: *Self, tree: Ast, index: Node.Index) (Error || Allocator.Error)!Node.Index {
    if (index == 0) return 0;
    var node = tree.node(index);

    switch (node.tag) {
        .comment => {},
        .span => {
            const scratch_top = self.scratch.items.len;
            defer self.scratch.shrinkRetainingCapacity(scratch_top);
            for (tree.spanToList(index)) |i| {
                try self.scratch.append(self.allocator, try self.visit(tree, i));
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
            global_var.name = try self.getOrPutIdent(.decl, tree.identifier(global_var.name), node.src_offset);
            global_var.attrs = try self.visit(tree, global_var.attrs);
            global_var.template_list = try self.visit(tree, global_var.template_list);
            global_var.type = try self.visit(tree, global_var.type);
            node.lhs = try self.addExtra(global_var);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .override => {
            var override = tree.extraData(Node.Override, node.lhs);
            override.name = try self.getOrPutIdent(.decl, tree.identifier(override.name), node.src_offset);
            override.attrs = try self.visit(tree, override.attrs);
            override.type = try self.visit(tree, override.type);
            node.lhs = try self.addExtra(override);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .@"fn" => {
            var header = tree.extraData(Node.FnHeader, node.lhs);
            header.name = try self.getOrPutIdent(.decl, tree.identifier(header.name), node.src_offset);
            header.attrs = try self.visit(tree, header.attrs);
            header.params = try self.visit(tree, header.params);
            header.return_attrs = try self.visit(tree, header.return_attrs);
            header.return_type = try self.visit(tree, header.return_type);
            node.lhs = try self.addExtra(header);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .fn_param => {
            var param = tree.extraData(Node.FnParam, node.rhs);
            param.name = try self.getOrPutIdent(.decl, tree.identifier(param.name), node.src_offset);
            param.type = try self.visit(tree, param.type);
            node.lhs = try self.visit(tree, node.lhs);
            node.rhs = try self.addExtra(param);
        },
        .@"var" => {
            var extra = tree.extraData(Node.Var, node.lhs);
            extra.name = try self.getOrPutIdent(.decl, tree.identifier(extra.name), node.src_offset);
            extra.template_list = try self.visit(tree, extra.template_list);
            extra.type = try self.visit(tree, extra.type);
            node.lhs = try self.addExtra(extra);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .@"for" => {
            var header = tree.extraData(Node.ForHeader, node.lhs);
            header.attrs = try self.visit(tree, header.attrs);
            header.cond = try self.visit(tree, header.cond);
            header.init = try self.visit(tree, header.init);
            header.update = try self.visit(tree, header.update);
            node.lhs = try self.addExtra(header);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .import_alias => {
            const sym = tree.identifier(node.lhs);
            node.lhs = try self.getOrPutIdent(.token, sym, node.src_offset);
            if (node.rhs != 0) {
                const alias = tree.identifier(node.rhs);
                node.rhs = try self.getOrPutIdent(.token, alias, node.src_offset);

                const scope_index = self.scopes.items.len - 1;
                var scope: *Scope = &self.scopes.items[scope_index];
                const existing = scope.get(sym).?;
                try scope.put(alias, existing);
            }
        },
        .type_alias => {
            node.lhs = try self.getOrPutIdent(.decl, tree.identifier(node.lhs), node.src_offset);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .type, .ident => {
            node.lhs = try self.getOrPutIdent(.ref, tree.identifier(node.lhs), node.src_offset);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .@"struct" => {
            node.lhs = try self.getOrPutIdent(.decl, tree.identifier(node.lhs), node.src_offset);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .import => {
            node.lhs = try self.visit(tree, node.lhs);
            node.rhs = try self.getOrPutIdent(.token, tree.identifier(node.rhs), node.src_offset);
        },
        .field_access => {
            node.lhs = try self.visit(tree, node.lhs);
            node.rhs = try self.getOrPutIdent(.token, tree.identifier(node.rhs), node.src_offset);
        },
        .number => {
            node.lhs = try self.getOrPutIdent(.token, tree.identifier(node.lhs), node.src_offset);
        },
        .@"const", .let => {
            node.lhs = try self.typedIdent(.decl, tree, node.lhs, node.src_offset);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .attribute => {
            const attr: Node.Attribute = @enumFromInt(node.lhs);
            switch (attr) {
                .@"const", .compute, .fragment, .invariant, .must_use, .vertex => {},
                .diagnostic => {
                    const diagnostic = try self.diagnosticControl(tree, node.rhs);
                    node.rhs = try self.addExtra(diagnostic);
                },
                .interpolate => {
                    var interpolate = tree.extraData(Node.Interpolation, node.rhs);
                    interpolate.type = try self.visit(tree, interpolate.type);
                    interpolate.sampling_expr = try self.visit(tree, interpolate.sampling_expr);
                    node.rhs = try self.addExtra(interpolate);
                },
                .@"align",
                .binding,
                .builtin,
                .group,
                .id,
                .location,
                .size,
                => {
                    node.rhs = try self.visit(tree, node.rhs);
                },
                .workgroup_size => {
                    var workgroup_size = tree.extraData(Node.WorkgroupSize, node.rhs);
                    workgroup_size.x = try self.visit(tree, workgroup_size.x);
                    workgroup_size.y = try self.visit(tree, workgroup_size.y);
                    workgroup_size.z = try self.visit(tree, workgroup_size.z);
                    node.rhs = try self.addExtra(workgroup_size);
                },
            }
        },
        .compound => {
            node.lhs = try self.visit(tree, node.lhs);
            try self.pushScope();
            node.rhs = try self.visit(tree, node.rhs);
            self.popScope();
        },
        .struct_member => {
            node.lhs = try self.visit(tree, node.lhs);
            node.rhs = try self.typedIdent(.token, tree, node.rhs, node.src_offset);
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
            node.lhs = try self.visit(tree, node.lhs);
            node.rhs = try self.visit(tree, node.rhs);
        },
        .true, .false, .@"break", .@"continue", .discard => {},
    }

    return try self.addNode(node);
}
