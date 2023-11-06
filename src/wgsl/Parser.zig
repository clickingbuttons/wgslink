const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");
const Node = @import("Node.zig");

const Self = @This();
const Error = error{ OutOfMemory, Parsing };

allocator: std.mem.Allocator,
source: [:0]const u8,
// May mutate for template tags
tokens: Ast.TokenList,
tok_i: Token.Index = 0,
errors: std.ArrayListUnmanaged(Ast.Error) = .{},
// Tells us if f16 is a type or ident.
enable_extensions: Node.EnableExtensions = .{},
// Currently unused but could add additional syntax in the future.
lang_extensions: Node.LangExtensions = .{},
nodes: Ast.NodeList = .{},
extra: std.ArrayListUnmanaged(Node.Index) = .{},
scratch: std.ArrayListUnmanaged(Node.Index) = .{},

/// global_directive* global_decl*
pub fn parseTranslationUnit(p: *Self) error{OutOfMemory}!void {
    try p.parameterizeTemplates();
    // Root node must be index 0.
    p.nodes.appendAssumeCapacity(.{
        .tag = .span,
        .main_token = 0,
    });
    // So that extra nodes are never `0`.
    try p.extra.append(p.allocator, 0);

    while (p.peekToken(.tag, 0) != .eof) {
        const directive = try p.globalDirectiveRecoverable();
        if (directive == 0) break;
        try p.scratch.append(p.allocator, directive);
    }

    while (p.peekToken(.tag, 0) != .eof) {
        const decl = try p.expectGlobalDeclRecoverable();
        if (decl == 0) continue;
        try p.scratch.append(p.allocator, decl);
    }

    try p.extra.appendSlice(p.allocator, p.scratch.items);
    p.nodes.items(.lhs)[0] = @intCast(p.extra.items.len - p.scratch.items.len);
    p.nodes.items(.rhs)[0] = @intCast(p.extra.items.len);
}

/// Disambiguate templates (since WGSL chose < and >)
/// https://gpuweb.github.io/gpuweb/wgsl/#template-lists-sec
fn parameterizeTemplates(p: *Self) error{OutOfMemory}!void {
    const UnclosedCandidate = struct {
        token_tag: *Token.Tag,
        depth: u32,
    };
    var discovered_tmpls = std.BoundedArray(UnclosedCandidate, 16).init(0) catch unreachable;
    var depth: u32 = 0;

    var i: u32 = 0;
    while (i < p.tokens.len) : (i += 1) {
        switch (p.tokens.items(.tag)[i]) {
            .ident,
            .k_var,
            .k_bitcast,
            .k_array,
            .k_atomic,
            .k_ptr,
            .k_vec2,
            .k_vec3,
            .k_vec4,
            .k_mat2x2,
            .k_mat2x3,
            .k_mat2x4,
            .k_mat3x2,
            .k_mat3x3,
            .k_mat3x4,
            .k_mat4x2,
            .k_mat4x3,
            .k_mat4x4,
            .k_texture_1d,
            .k_texture_2d,
            .k_texture_2d_array,
            .k_texture_3d,
            .k_texture_cube,
            .k_texture_cube_array,
            .k_texture_storage_1d,
            .k_texture_storage_2d,
            .k_texture_storage_2d_array,
            .k_texture_storage_3d,
            .k_texture_multisampled_2d,
            => if (p.tokens.items(.tag)[i + 1] == .@"<") {
                discovered_tmpls.append(.{
                    .token_tag = &p.tokens.items(.tag)[i + 1],
                    .depth = depth,
                }) catch {
                    try p.errors.append(p.allocator, Ast.Error{
                        .tag = .deep_template,
                        .token = i,
                    });
                    return Error.OutOfMemory;
                };
                i += 1;
            },
            .@">" => {
                if (discovered_tmpls.len > 0 and
                    discovered_tmpls.get(discovered_tmpls.len - 1).depth == depth)
                {
                    discovered_tmpls.pop().token_tag.* = .template_args_start;
                    p.tokens.items(.tag)[i] = .template_args_end;
                }
            },
            .@">>" => {
                if (discovered_tmpls.len > 0 and
                    discovered_tmpls.get(discovered_tmpls.len - 1).depth == depth)
                {
                    discovered_tmpls.pop().token_tag.* = .template_args_start;
                    discovered_tmpls.pop().token_tag.* = .template_args_start;

                    p.tokens.items(.tag)[i] = .template_args_end;
                    try p.tokens.insert(p.allocator, i, Token{
                        .tag = .template_args_end,
                        .loc = .{
                            .start = p.tokens.items(.loc)[i].start + 1,
                            .end = p.tokens.items(.loc)[i].end + 1,
                        },
                    });
                }
            },
            .@"(", .@"{" => depth += 1,
            .@")", .@"}" => {
                while (discovered_tmpls.len > 0 and
                    discovered_tmpls.get(discovered_tmpls.len - 1).depth == depth) _ = discovered_tmpls.pop();

                if (depth > 0) depth -= 1;
            },
            .@";", .@":", .@"[" => {
                depth = 0;
                discovered_tmpls.resize(0) catch unreachable;
            },
            .@"||", .@"&&" => {
                while (discovered_tmpls.len > 0 and
                    discovered_tmpls.get(discovered_tmpls.len - 1).depth == depth) _ = discovered_tmpls.pop();
            },
            else => {},
        }
    }
}

fn listToSpan(p: *Self, list: []const Node.Index) Error!Node.Index {
    if (list.len == 0) return 0;
    try p.extra.appendSlice(p.allocator, @ptrCast(list));
    return p.addNode(.{
        .tag = .span,
        .main_token = undefined,
        .lhs = @intCast(p.extra.items.len - list.len),
        .rhs = @intCast(p.extra.items.len),
    });
}

fn spanToList(self: Self, i: Node.Index) []const Node.Index {
    std.debug.assert(self.nodes.items(.tag)[i] == .span);
    return @ptrCast(self.extra.items[self.nodes.items(.lhs)[i]..self.nodes.items(.rhs)[i]]);
}

fn addNode(p: *Self, node: Node) error{OutOfMemory}!Node.Index {
    const i: Node.Index = @intCast(p.nodes.len);
    try p.nodes.append(p.allocator, node);
    return i;
}

fn addExtra(p: *Self, extra: anytype) error{OutOfMemory}!Node.Index {
    const fields = std.meta.fields(@TypeOf(extra));
    try p.extra.ensureUnusedCapacity(p.allocator, fields.len);
    const result: Node.Index = @intCast(p.extra.items.len);
    inline for (fields) |field| {
        comptime std.debug.assert(field.type == Node.Index or field.type == Token.Index);
        p.extra.appendAssumeCapacity(@field(extra, field.name));
    }
    return result;
}

fn peekToken(
    p: Self,
    comptime field: Ast.TokenList.Field,
    offset: usize,
) std.meta.fieldInfo(Token, field).type {
    return p.tokens.items(field)[p.tok_i + offset];
}

fn advanceToken(p: *Self) Token.Index {
    const prev = p.tok_i;
    p.tok_i = @min(prev + 1, p.tokens.len - 1);
    return prev;
}

fn isKeyword(tag: Token.Tag) bool {
    const tag_name = @tagName(tag);
    return tag_name.len > 2 and std.mem.eql(u8, tag_name[0..2], "k_");
}

fn eatToken(p: *Self, tag: Token.Tag) ?Token.Index {
    return if (p.peekToken(.tag, 0) == tag) p.advanceToken() else null;
}

fn eatKeyword(p: *Self) ?Token.Index {
    return if (isKeyword(p.peekToken(.tag, 0))) p.advanceToken() else null;
}

fn expectToken(p: *Self, tag: Token.Tag) Error!Token.Index {
    const token = p.advanceToken();
    if (p.tokens.items(.tag)[token] == tag) return token;

    try p.errors.append(p.allocator, Ast.Error{
        .tag = .expected_token,
        .token = token,
        .expected_tag = tag,
    });
    return Error.Parsing;
}

fn tokenSource(p: Self, i: Token.Index) []const u8 {
    const loc = p.tokens.items(.loc)[i];
    return p.source[loc.start..loc.end];
}

fn expectAttribEnd(p: *Self) Error!void {
    _ = p.eatToken(.@",");
    _ = try p.expectToken(.@")");
}

const Diagnostic = struct { severity: Token.Index, rule: Node.Index };
/// '(' severity_control_name ',' diagnostic_rule_name attrib_end
/// diagnostic_rule_name:
///   | diagnostic_name_token
///   | diagnostic_name_token '.' diagnostic_name_token
fn expectDiagnosticControl(p: *Self) Error!Diagnostic {
    _ = try p.expectToken(.@"(");
    const severity = try p.expectToken(.ident);
    const str = p.tokenSource(severity);
    _ = std.meta.stringToEnum(Node.Severity, str) orelse {
        try p.errors.append(p.allocator, Ast.Error{
            .tag = .invalid_severity,
            .token = severity,
        });
        return Error.Parsing;
    };
    _ = try p.expectToken(.@",");

    var rule = Node.DiagnosticRule{
        .name = try p.expectToken(.ident),
    };
    if (p.eatToken(.@".")) |_| rule.field = try p.expectToken(.ident);
    try p.expectAttribEnd();

    return .{ .severity = severity, .rule = try p.addExtra(rule) };
}

// 'diagnostic' diagnostic_control ';'
fn diagnosticDirective(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_diagnostic) orelse return null;
    const diag = try p.expectDiagnosticControl();
    _ = try p.expectToken(.@";");
    return try p.addNode(.{
        .main_token = main_token,
        .tag = .diagnostic_directive,
        .lhs = diag.severity,
        .rhs = diag.rule,
    });
}

/// | enable_extension_list :
///   enable_extension_name ( ',' enable_extension_name ) * ',' ?
/// | software_extension_list :
///   software_extension_name ( `','` software_extension_name ) * `','` ?
fn identTokenList(p: *Self, comptime p_field: []const u8) Error!Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const tok = p.eatKeyword() orelse try p.expectToken(.ident);
        try p.scratch.append(p.allocator, tok);
        if (p.eatToken(.@",") == null) break;

        const str = p.tokenSource(tok);
        var field = @field(p, p_field);
        inline for (@typeInfo(@TypeOf(field)).Struct.fields) |f| {
            if (std.mem.eql(u8, f.name, str)) {
                @field(field, f.name) = true;
                break;
            }
        }
    }
    _ = p.eatToken(.@",");
    return try p.listToSpan(p.scratch.items[scratch_top..]);
}

/// 'enable' enable_extension_list ';'
fn enableDirective(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_enable) orelse return null;
    const extensions = try p.identTokenList("enable_extensions");
    _ = try p.expectToken(.@";");
    return try p.addNode(.{
        .main_token = main_token,
        .tag = .enable_directive,
        .lhs = extensions,
    });
}

/// 'requires' software_extension_list ';'
fn requiresDirective(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_requires) orelse return null;
    const extensions = try p.identTokenList("lang_extensions");
    _ = try p.expectToken(.@";");
    return try p.addNode(.{
        .main_token = main_token,
        .tag = .requires_directive,
        .lhs = extensions,
    });
}

/// diagnostic_directive | enable_directive | requires_directive
fn globalDirective(p: *Self) Error!Node.Index {
    while (p.eatToken(.@";")) |_| {}
    return (try p.diagnosticDirective() orelse
        try p.enableDirective() orelse
        try p.requiresDirective() orelse 0);
}

fn findNextGlobalDirective(p: *Self) void {
    while (true) {
        switch (p.peekToken(.tag, 0)) {
            .k_enable, .k_requires, .k_diagnostic, .eof => return,
            .@";" => {
                _ = p.advanceToken();
                return;
            },
            else => _ = p.advanceToken(),
        }
    }
}

fn globalDirectiveRecoverable(p: *Self) error{OutOfMemory}!Node.Index {
    return p.globalDirective() catch |err| switch (err) {
        Error.Parsing => {
            p.findNextGlobalDirective();
            return 0;
        },
        error.OutOfMemory => error.OutOfMemory,
    };
}

/// ;
/// | global_variable_decl ;
/// | override_decl ;
/// | const_decl ;
/// | function_decl
/// | type_alias_decl ;
/// | struct_decl
/// | const_assert_statement ;
fn expectGlobalDecl(p: *Self) Error!Node.Index {
    if (p.eatToken(.@";")) |_| return 0; // Skip empty decls

    const attrs = try p.attributeList();
    if (try p.globalVariableDecl(attrs) orelse try p.globalOverrideDecl(attrs)) |node| {
        _ = try p.expectToken(.@";");
        return node;
    }
    if (try p.fnDecl(attrs)) |node| return node;

    if (attrs != 0) {
        const first_attr = p.spanToList(attrs)[0];
        try p.errors.append(p.allocator, Ast.Error{
            .tag = .invalid_attributes,
            .token = p.nodes.items(.main_token)[first_attr],
        });
        return Error.Parsing;
    }
    if (try p.constDecl() orelse
        try p.typeAliasDecl() orelse
        try p.constAssert() orelse
        try p.importDecl()) |node|
    {
        _ = try p.expectToken(.@";");
        return node;
    }

    if (try p.structDecl()) |node| return node;

    try p.errors.append(p.allocator, Ast.Error{
        .tag = .expected_global_decl,
        .token = p.tok_i,
    });
    return Error.Parsing;
}

fn findNextGlobalDecl(p: *Self) void {
    var level: u32 = 0;
    while (true) {
        switch (p.peekToken(.tag, 0)) {
            .k_fn,
            .k_var,
            .k_const,
            .k_override,
            .k_struct,
            .@"@",
            => {
                if (level == 0) return;
            },
            .@";" => {
                if (level == 0) {
                    _ = p.advanceToken();
                    return;
                }
            },
            .@"{",
            .@"[",
            .@"(",
            => {
                level += 1;
            },
            .@"}" => {
                if (level == 0) {
                    _ = p.advanceToken();
                    return;
                }
                level -= 1;
            },
            .@"]", .@")" => {
                if (level != 0) level -= 1;
            },
            .eof => return,
            else => {},
        }
        _ = p.advanceToken();
    }
}

fn expectGlobalDeclRecoverable(p: *Self) error{OutOfMemory}!Node.Index {
    return p.expectGlobalDecl() catch |err| switch (err) {
        Error.Parsing => {
            p.findNextGlobalDecl();
            return 0;
        },
        error.OutOfMemory => error.OutOfMemory,
    };
}

/// | '@' 'align' '(' expression attrib_end
/// | '@' 'binding' '(' expression attrib_end
/// | '@' 'builtin' '(' expression attrib_end
/// | '@' 'const'
/// | '@' 'diagnostic' diagnostic_control
/// | '@' 'group' '(' expression attrib_end
/// | '@' 'id' '(' expression attrib_end
/// | '@' 'interpolate' '(' expression attrib_end
/// | '@' 'interpolate' '(' expression ',' expression attrib_end
/// | '@' 'invariant'
/// | '@' 'location' '(' expression attrib_end
/// | '@' 'must_use'
/// | '@' 'size' '(' expression attrib_end
/// | '@' 'workgroup_size' '(' expression attrib_end
/// | '@' 'workgroup_size' '(' expression ',' expression attrib_end
/// | '@' 'workgroup_size' '(' expression ',' expression ',' expression attrib_end
/// | '@' 'vertex'
/// | '@' 'fragment'
/// | '@' 'compute'
fn attribute(p: *Self) Error!Node.Index {
    const attr_token = p.eatToken(.@"@") orelse return 0;
    const ident_token = p.eatToken(.k_diagnostic) orelse try p.expectToken(.ident);
    const str = p.tokenSource(ident_token);
    const tag = std.meta.stringToEnum(Node.Attribute, str) orelse {
        try p.errors.append(p.allocator, Ast.Error{
            .tag = .invalid_attribute,
            .token = ident_token,
        });
        return Error.Parsing;
    };

    var node = Node{
        .tag = undefined,
        .main_token = attr_token,
    };

    switch (tag) {
        .@"align" => {
            node.tag = .attr_align;
            _ = try p.expectToken(.@"(");
            node.lhs = try p.expectExpression();
            try p.expectAttribEnd();
        },
        .binding => {
            node.tag = .attr_binding;
            _ = try p.expectToken(.@"(");
            node.lhs = try p.expectExpression();
            try p.expectAttribEnd();
        },
        .builtin => {
            _ = try p.expectToken(.@"(");
            node.tag = .attr_builtin;
            node.lhs = try p.expectBuiltin();
            try p.expectAttribEnd();
        },
        .compute => node.tag = .attr_compute,
        .@"const" => node.tag = .attr_const,
        .diagnostic => {
            node.tag = .attr_diagnostic;
            const diag = try p.expectDiagnosticControl();
            node.lhs = diag.severity;
            node.rhs = diag.rule;
        },
        .fragment => node.tag = .attr_fragment,
        .group => {
            _ = try p.expectToken(.@"(");
            node.tag = .attr_group;
            node.lhs = try p.expectExpression();
            try p.expectAttribEnd();
        },
        .id => {
            _ = try p.expectToken(.@"(");
            node.tag = .attr_id;
            node.lhs = try p.expectExpression();
            try p.expectAttribEnd();
        },
        .interpolate => {
            _ = try p.expectToken(.@"(");
            node.tag = .attr_interpolate;
            node.lhs = try p.expectInterpolationType();
            if (p.eatToken(.@",") != null and p.peekToken(.tag, 0) != .@")") {
                node.rhs = try p.expectInterpolationSample();
            }
            try p.expectAttribEnd();
        },
        .invariant => node.tag = .attr_invariant,
        .location => {
            _ = try p.expectToken(.@"(");
            node.tag = .attr_location;
            node.lhs = try p.expectExpression();
            try p.expectAttribEnd();
        },
        .must_use => node.tag = .attr_must_use,
        .size => {
            _ = try p.expectToken(.@"(");
            node.tag = .attr_size;
            node.lhs = try p.expectExpression();
            try p.expectAttribEnd();
        },
        .vertex => node.tag = .attr_vertex,
        .workgroup_size => {
            _ = try p.expectToken(.@"(");
            node.tag = .attr_workgroup_size;
            var workgroup_size = Node.WorkgroupSize{
                .x = try p.expectExpression(),
            };
            if (p.eatToken(.@",") != null and p.peekToken(.tag, 0) != .@")") {
                workgroup_size.y = try p.expectExpression();

                if (p.eatToken(.@",") != null and p.peekToken(.tag, 0) != .@")") {
                    workgroup_size.z = try p.expectExpression();
                }
            }
            try p.expectAttribEnd();
            node.lhs = try p.addExtra(workgroup_size);
        },
    }

    return try p.addNode(node);
}

/// attribute *
fn attributeList(p: *Self) Error!Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const attr = try p.attribute();
        if (attr == 0) break;
        try p.scratch.append(p.allocator, attr);
    }
    return try p.listToSpan(p.scratch.items[scratch_top..]);
}

/// ident
fn expectBuiltin(p: *Self) Error!Token.Index {
    if (p.eatToken(.ident)) |t| {
        if (std.meta.stringToEnum(Node.Builtin, p.tokenSource(t))) |_| return t;
    }

    try p.errors.append(p.allocator, Ast.Error{ .tag = .invalid_builtin, .token = p.tok_i });
    return Error.Parsing;
}

/// ident
fn expectInterpolationType(p: *Self) Error!Token.Index {
    if (p.eatToken(.ident)) |t| {
        if (std.meta.stringToEnum(Node.InterpolationType, p.tokenSource(t))) |_| return t;
    }

    try p.errors.append(p.allocator, Ast.Error{
        .tag = .invalid_interpolation_type,
        .token = p.tok_i,
    });
    return Error.Parsing;
}

/// ident
fn expectInterpolationSample(p: *Self) Error!Token.Index {
    if (p.eatToken(.ident)) |t| {
        if (std.meta.stringToEnum(Node.InterpolationSample, p.tokenSource(t))) |_| return t;
    }

    try p.errors.append(p.allocator, Ast.Error{
        .tag = .invalid_interpolation_sample,
        .token = p.tok_i,
    });
    return Error.Parsing;
}

const Ident = struct {
    name: Token.Index,
    type: Node.Index,
    initializer: Node.Index,
};
/// ident ( ':' type_specifier ) ? ( = expression ) ?
/// Type specifier or expression must be included.
fn expectOptionallyTypedIdentWithInitializer(p: *Self) Error!Ident {
    const name = try p.expectToken(.ident);
    var var_type: Node.Index = 0;
    if (p.eatToken(.@":")) |_| var_type = try p.expectTypeSpecifier();
    var initializer: Node.Index = 0;
    if (p.eatToken(.@"=")) |_| initializer = try p.expectExpression();
    if (initializer == 0 and var_type == 0) {
        try p.errors.append(p.allocator, Ast.Error{
            .tag = .invalid_initializer,
            .token = name,
        });
        return Error.Parsing;
    }

    return .{
        .name = name,
        .type = var_type,
        .initializer = initializer,
    };
}

/// attribute * variable_decl ( '=' expression ) ?
/// variable_decl :
///   'var' _disambiguate_template template_list ? optionally_typed_ident
fn globalVariableDecl(p: *Self, attrs: Node.Index) Error!?Node.Index {
    const var_token = p.eatToken(.k_var) orelse return null;

    // We're more strict here and don't allow just any template list.
    // This is good because we'll need to use these later for metadata.
    var addr_space: Token.Index = 0;
    var access_mode: Token.Index = 0;
    if (p.eatToken(.template_args_start)) |_| {
        addr_space = try p.expectAddressSpace();
        if (p.eatToken(.@",")) |_| access_mode = try p.expectAccessMode();
        _ = try p.expectToken(.template_args_end);
    }

    const ident = try p.expectOptionallyTypedIdentWithInitializer();

    const extra = try p.addExtra(Node.GlobalVar{
        .attrs = attrs,
        .name = ident.name,
        .addr_space = addr_space,
        .access_mode = access_mode,
        .type = ident.type,
    });
    return try p.addNode(.{
        .tag = .global_var,
        .main_token = var_token,
        .lhs = extra,
        .rhs = ident.initializer,
    });
}

/// attribute * 'override' optionally_typed_ident ( '=' expression ) ?
fn globalOverrideDecl(p: *Self, attrs: Node.Index) Error!?Node.Index {
    const override_token = p.eatToken(.k_override) orelse return null;
    const ident = try p.expectOptionallyTypedIdentWithInitializer();

    const extra = try p.addExtra(Node.Override{
        .attrs = attrs,
        .type = ident.type,
    });
    return try p.addNode(.{
        .tag = .override,
        .main_token = override_token,
        .lhs = extra,
        .rhs = ident.initializer,
    });
}

/// 'alias' ident '=' type_specifier
fn typeAliasDecl(p: *Self) Error!?Node.Index {
    const type_token = p.eatToken(.k_alias) orelse return null;
    _ = try p.expectToken(.ident);
    _ = try p.expectToken(.@"=");
    const value = try p.expectTypeSpecifier();
    return try p.addNode(.{
        .tag = .type_alias,
        .main_token = type_token,
        .lhs = value,
    });
}

/// attribute * member_ident `':'` type_specifier
fn structMember(p: *Self, attrs: Node.Index) Error!?Node.Index {
    const name_token = p.eatToken(.ident) orelse return null;
    _ = try p.expectToken(.@":");
    const member_type = try p.expectTypeSpecifier();
    return try p.addNode(.{
        .tag = .struct_member,
        .main_token = name_token,
        .lhs = attrs,
        .rhs = member_type,
    });
}

/// 'struct' ident `'{'` struct_member ( `','` struct_member ) * `','` ? `'}'`
fn structDecl(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_struct) orelse return null;
    const name_token = try p.expectToken(.ident);

    _ = try p.expectToken(.@"{");
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const attrs = try p.attributeList();
        const member = try p.structMember(attrs) orelse {
            if (attrs != 0) {
                try p.errors.append(p.allocator, Ast.Error{
                    .tag = .expected_struct_member,
                    .token = p.tok_i,
                });
                return Error.Parsing;
            }
            break;
        };
        try p.scratch.append(p.allocator, member);
        _ = p.eatToken(.@",");
    }
    _ = try p.expectToken(.@"}");

    const members = p.scratch.items[scratch_top..];
    if (members.len == 0) {
        try p.errors.append(p.allocator, Ast.Error{
            .tag = .empty_struct,
            .token = name_token,
        });
        return Error.Parsing;
    }

    return try p.addNode(.{
        .tag = .@"struct",
        .main_token = main_token,
        .lhs = try p.listToSpan(members),
    });
}

/// 'const_assert' expression
fn constAssert(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_const_assert) orelse return null;
    const expr = try p.expectExpression();
    return try p.addNode(.{
        .tag = .const_assert,
        .main_token = main_token,
        .lhs = expr,
    });
}

/// 'import' import_list 'from' string_literal ';'?
fn importDecl(p: *Self) Error!?Node.Index {
    const import_token = p.eatToken(.k_import) orelse return null;
    const imports = try p.importList();
    _ = try p.expectToken(.k_from);
    const mod_token = try p.expectToken(.string_literal);

    return try p.addNode(.{
        .tag = .import,
        .main_token = import_token,
        .lhs = imports,
        .rhs = mod_token,
    });
}

/// ident '(' param_list ? ')' ( '->' attribute * template_elaborated_ident ) ?
fn fnHeader(p: *Self, attrs: Node.Index) Error!Node.Index {
    _ = try p.expectToken(.ident);
    _ = try p.expectToken(.@"(");
    const params = try p.parameterList() orelse 0;
    _ = try p.expectToken(.@")");

    var return_attrs: Node.Index = 0;
    var return_type: Node.Index = 0;
    if (p.eatToken(.@"->")) |_| {
        return_attrs = try p.attributeList();
        return_type = try p.expectTypeSpecifier();
    }

    return try p.addExtra(Node.FnProto{
        .attrs = attrs,
        .params = params,
        .return_attrs = return_attrs,
        .return_type = return_type,
    });
}

/// attribute* 'fn' function_header compound_statement
fn fnDecl(p: *Self, attrs: Node.Index) Error!?Node.Index {
    const fn_token = p.eatToken(.k_fn) orelse return null;
    const fn_header = try p.fnHeader(attrs);
    const body = try p.expectCompoundStatement(try p.attributeList());
    return try p.addNode(.{
        .tag = .@"fn",
        .main_token = fn_token,
        .lhs = fn_header,
        .rhs = body,
    });
}

// '{' ident (',' ident)* ','? '}'
fn importList(p: *Self) Error!Node.Index {
    _ = p.eatToken(.@"{") orelse return 0;
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const main_token = try p.expectToken(.ident);
        const imp = try p.addNode(.{
            .main_token = main_token,
            .tag = .ident,
        });
        try p.scratch.append(p.allocator, imp);
        if (p.eatToken(.@",") == null) break;
    }
    _ = try p.expectToken(.@"}");
    const imports = p.scratch.items[scratch_top..];
    return try p.listToSpan(imports);
}

/// attribute * ident ':' type_specifier
fn parameter(p: *Self) Error!Node.Index {
    const attrs = try p.attributeList();
    const main_token = p.eatToken(.ident) orelse {
        if (attrs != 0) {
            try p.errors.append(p.allocator, Ast.Error{
                .tag = .expected_function_parameter,
                .token = p.tok_i,
            });
            return Error.Parsing;
        }
        return 0;
    };
    _ = try p.expectToken(.@":");
    const param_type = try p.expectTypeSpecifier();
    return try p.addNode(.{
        .tag = .fn_param,
        .main_token = main_token,
        .lhs = attrs,
        .rhs = param_type,
    });
}

/// param ( ',' param ) * ',' ?
fn parameterList(p: *Self) Error!?Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const param = try p.parameter();
        if (param != 0) try p.scratch.append(p.allocator, param);
        if (p.eatToken(.@",") == null) break;
    }
    return try p.listToSpan(p.scratch.items[scratch_top..]);
}

/// | ';'
/// | if_statement
/// | switch_statement
/// | loop_statement
/// | for_statement
/// | while_statement
/// | compound_statement
/// | return_statement ';'
/// | func_call_statement ';'
/// | variable_or_value_statement ';'
/// | break_statement ';'
/// | continue_statement ';'
/// | 'discard' ';'
/// | variable_updating_statement ';'
/// | const_assert_statement ';'
fn statement(p: *Self) Error!?Node.Index {
    if (p.eatToken(.@";")) |_| return null;

    const attrs = try p.attributeList();
    if (try p.loopStatement(attrs) orelse
        try p.compoundStatement(attrs)) |node| return node;

    if (attrs != 0) {
        const first_attr = p.spanToList(attrs)[0];
        try p.errors.append(p.allocator, Ast.Error{
            .tag = .invalid_attributes,
            .token = p.nodes.items(.main_token)[first_attr],
        });
        return Error.Parsing;
    }

    if (try p.ifStatement() orelse
        try p.switchStatement() orelse
        try p.forStatement() orelse
        try p.whileStatement()) |node| return node;
    if (try p.returnStatement() orelse
        try p.callExpr() orelse
        try p.variableOrValueStatement() orelse
        try p.breakStatement() orelse
        try p.continueStatement() orelse
        try p.discardStatement() orelse
        try p.varUpdateStatement() orelse
        try p.constAssert()) |node|
    {
        _ = try p.expectToken(.@";");
        return node;
    }

    return null;
}

fn findNextStmt(p: *Self) void {
    var level: Node.Index = 0;
    while (true) {
        switch (p.peekToken(.tag, 0)) {
            .@";" => {
                if (level == 0) {
                    _ = p.advanceToken();
                    return;
                }
            },
            .@"{" => {
                level += 1;
            },
            .@"}" => {
                if (level == 0) {
                    _ = p.advanceToken();
                    return;
                }
                level -= 1;
            },
            .eof => return,
            else => {},
        }
        _ = p.advanceToken();
    }
}

fn statementRecoverable(p: *Self) Error!?Node.Index {
    while (true) {
        return p.statement() catch |err| switch (err) {
            Error.Parsing => {
                p.findNextStmt();
                switch (p.peekToken(.tag, 0)) {
                    .@"}" => return null,
                    .eof => return Error.Parsing,
                    else => continue,
                }
            },
            error.OutOfMemory => error.OutOfMemory,
        };
    }
}

/// | variable_decl
/// | variable_decl '=' expression
/// | 'let' optionally_typed_ident '=' expression
/// | 'const' optionally_typed_ident '=' expression
fn variableOrValueStatement(p: *Self) Error!?Node.Index {
    return try p.varDecl() orelse try p.letDecl() orelse try p.constDecl();
}

/// 'return' expression ?
fn returnStatement(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_return) orelse return null;
    const expr = try p.expression() orelse 0;
    return try p.addNode(.{
        .tag = .@"return",
        .main_token = main_token,
        .lhs = expr,
    });
}

/// attribute * if_clause else_if_clause * else_clause ?
/// if_clause :
///  'if' expression compound_statement
/// else_if_clause :
///  'else' 'if' expression compound_statement
/// else_clause :
///  'else' compound_statement
fn ifStatement(p: *Self) Error!?Node.Index {
    const if_token = p.eatToken(.k_if) orelse return null;

    const cond = try p.expectExpression();
    const body = try p.expectCompoundStatement(try p.attributeList());
    const if_node = try p.addNode(.{
        .tag = .@"if",
        .main_token = if_token,
        .lhs = cond,
        .rhs = body,
    });

    if (p.eatToken(.k_else)) |else_token| {
        if (p.peekToken(.tag, 0) == .k_if) {
            // Instead of building a list of "else if" clauses, use recursion.
            const else_if = try p.ifStatement() orelse unreachable;
            return try p.addNode(.{
                .tag = .else_if,
                .main_token = else_token,
                .lhs = if_node,
                .rhs = else_if,
            });
        } else {
            const else_body = try p.expectCompoundStatement(try p.attributeList());
            return try p.addNode(.{
                .tag = .@"else",
                .main_token = else_token,
                .lhs = if_node,
                .rhs = else_body,
            });
        }
    }

    return if_node;
}

/// attribute * '{' statement * last_statement ? '}'
fn compoundStatementExtra(p: *Self, attrs: Node.Index, last_statement: anytype) Error!?Node.Index {
    const main_token = p.eatToken(.@"{") orelse return null;
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        if (try p.statement()) |s| try p.scratch.append(p.allocator, s) else break;
    }
    if (try last_statement(p)) |s| try p.scratch.append(p.allocator, s);
    _ = try p.expectToken(.@"}");
    return try p.addNode(.{
        .tag = .compound_statement,
        .main_token = main_token,
        .lhs = attrs,
        .rhs = try p.listToSpan(p.scratch.items[scratch_top..]),
    });
}

fn emptyNode(p: *Self) Error!?Node.Index {
    _ = p;
    return null;
}

/// attribute * '{' statement * '}'
fn compoundStatement(p: *Self, attrs: Node.Index) Error!?Node.Index {
    return p.compoundStatementExtra(attrs, emptyNode);
}

fn expectCompoundStatement(p: *Self, attrs: Node.Index) Error!Node.Index {
    return try p.compoundStatement(attrs) orelse {
        try p.errors.append(p.allocator, Ast.Error{
            .tag = .expected_compound_statement,
            .token = p.tok_i,
        });
        return Error.Parsing;
    };
}

/// 'break'
fn breakStatement(p: *Self) Error!?Node.Index {
    if (p.peekToken(.tag, 0) == .k_break and p.peekToken(.tag, 1) != .k_if) {
        return try p.addNode(.{ .tag = .@"break", .main_token = p.advanceToken() });
    }
    return null;
}

/// 'break' 'if' expression ';'
fn breakIfStatement(p: *Self) Error!?Node.Index {
    if (p.peekToken(.tag, 0) == .k_break and
        p.peekToken(.tag, 1) == .k_if)
    {
        const main_token = p.advanceToken();
        _ = p.advanceToken();
        const cond = try p.expectExpression();
        _ = try p.expectToken(.@";");
        return try p.addNode(.{
            .tag = .break_if,
            .main_token = main_token,
            .lhs = cond,
        });
    }
    return null;
}

/// 'continue'
fn continueStatement(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_continue) orelse return null;
    return try p.addNode(.{ .tag = .@"continue", .main_token = main_token });
}

/// attribute * '{' statement * break_if_statement ? '}'
fn continuingCompoundStatement(p: *Self) Error!?Node.Index {
    return p.compoundStatementExtra(try p.attributeList(), breakIfStatement);
}

fn expectContinuingCompoundStatement(p: *Self) Error!Node.Index {
    return try p.continuingCompoundStatement() orelse {
        try p.errors.append(p.allocator, Ast.Error{
            .tag = .expected_continuing_compound_statement,
            .token = p.tok_i,
        });
        return Error.Parsing;
    };
}

/// 'continuing' continuing_compound_statement
fn continuingStatement(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_continuing) orelse return null;
    const body = try p.expectContinuingCompoundStatement();
    return try p.addNode(.{
        .tag = .continuing,
        .main_token = main_token,
        .lhs = body,
    });
}

fn discardStatement(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_discard) orelse return null;
    return try p.addNode(.{ .tag = .discard, .main_token = main_token });
}

fn forStatement(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_for) orelse return null;
    _ = try p.expectToken(.@"(");

    // for init
    const for_init = try p.callExpr() orelse
        try p.varDecl() orelse
        try p.constDecl() orelse
        try p.letDecl() orelse
        try p.varUpdateStatement() orelse
        0;
    _ = try p.expectToken(.@";");

    const for_cond = try p.expression() orelse 0;
    _ = try p.expectToken(.@";");

    // for update
    const for_update = try p.callExpr() orelse
        try p.varUpdateStatement() orelse
        0;

    _ = try p.expectToken(.@")");
    const body = try p.expectCompoundStatement(try p.attributeList());

    const extra = try p.addExtra(Node.ForHeader{
        .init = for_init,
        .cond = for_cond,
        .update = for_update,
    });
    return try p.addNode(.{
        .tag = .@"for",
        .main_token = main_token,
        .lhs = extra,
        .rhs = body,
    });
}

// attribute * 'loop' attribute * '{' statement * continuing_statement ? '}'
fn loopStatement(p: *Self, attrs: Node.Index) Error!?Node.Index {
    const main_token = p.eatToken(.k_loop) orelse return null;
    const compound = try p.compoundStatementExtra(try p.attributeList(), continuingStatement) orelse {
        try p.errors.append(p.allocator, Ast.Error{
            .tag = .expected_compound_statement,
            .token = p.tok_i,
        });
        return Error.Parsing;
    };
    return try p.addNode(.{
        .tag = .loop,
        .main_token = main_token,
        .lhs = attrs,
        .rhs = compound,
    });
}

fn switchStatement(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_switch) orelse return null;

    const expr = try p.expectExpression();
    _ = try p.expectToken(.@"{");

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        if (p.eatToken(.k_default)) |default_token| {
            _ = p.eatToken(.@":");
            const default_body = try p.expectCompoundStatement(try p.attributeList());
            try p.scratch.append(p.allocator, try p.addNode(.{
                .tag = .switch_default,
                .main_token = default_token,
                .rhs = default_body,
            }));
        } else if (p.eatToken(.k_case)) |case_token| {
            const cases_scratch_top = p.scratch.items.len;

            var has_default = false;
            while (true) {
                const case_expr = try p.expression() orelse {
                    if (p.eatToken(.k_default)) |_| has_default = true;
                    break;
                };
                _ = p.eatToken(.@",");
                try p.scratch.append(p.allocator, case_expr);
            }
            const case_expr_list = p.scratch.items[cases_scratch_top..];

            _ = p.eatToken(.@":");
            const default_body = try p.expectCompoundStatement(try p.attributeList());

            p.scratch.shrinkRetainingCapacity(cases_scratch_top);
            try p.scratch.append(p.allocator, try p.addNode(.{
                .tag = if (has_default) .switch_case_default else .switch_case,
                .main_token = case_token,
                .lhs = if (case_expr_list.len == 0) 0 else try p.listToSpan(case_expr_list),
                .rhs = default_body,
            }));
        } else {
            break;
        }
    }

    _ = try p.expectToken(.@"}");

    const case_list = p.scratch.items[scratch_top..];
    return try p.addNode(.{
        .tag = .@"switch",
        .main_token = main_token,
        .lhs = expr,
        .rhs = try p.listToSpan(case_list),
    });
}

// | 'var' _disambiguate_template template_list ? optionally_typed_ident
// | 'var' _disambiguate_template template_list ? optionally_typed_ident '=' expression
fn varDecl(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_var) orelse return null;
    var addr_space: Token.Index = 0;
    var access_mode: Token.Index = 0;
    if (p.eatToken(.template_args_start)) |_| {
        addr_space = try p.expectAddressSpace();
        if (p.eatToken(.@",")) |_| access_mode = try p.expectAccessMode();
        _ = try p.expectToken(.template_args_end);
    }
    const ident = try p.expectOptionallyTypedIdentWithInitializer();
    const extra = try p.addExtra(Node.Var{
        .name = ident.name,
        .addr_space = addr_space,
        .access_mode = access_mode,
        .type = ident.type,
    });
    return try p.addNode(.{
        .tag = .@"var",
        .main_token = main_token,
        .lhs = extra,
        .rhs = ident.initializer,
    });
}

/// 'const' optionally_typed_ident '=' expression
fn constDecl(p: *Self) Error!?Node.Index {
    const const_token = p.eatToken(.k_const) orelse return null;
    const ident = try p.expectOptionallyTypedIdentWithInitializer();
    return try p.addNode(.{
        .tag = .@"const",
        .main_token = const_token,
        .lhs = ident.type,
        .rhs = ident.initializer,
    });
}

/// 'let' optionally_typed_ident '=' expression
fn letDecl(p: *Self) Error!?Node.Index {
    const const_token = p.eatToken(.k_let) orelse return null;

    _ = try p.expectToken(.ident);
    var const_type: Node.Index = 0;
    if (p.eatToken(.@":")) |_| {
        const_type = try p.expectTypeSpecifier();
    }

    _ = try p.expectToken(.@"=");
    const initializer = try p.expectExpression();
    return try p.addNode(.{
        .tag = .let,
        .main_token = const_token,
        .lhs = const_type,
        .rhs = initializer,
    });
}

/// | '_' '=' expression
/// | lhs_expression ( '=' | compound_assignment_operator ) expression
/// | lhs_expression '++'
/// | lhs_expression '--'
fn varUpdateStatement(p: *Self) Error!?Node.Index {
    if (p.eatToken(._)) |_| {
        const equal_token = try p.expectToken(.@"=");
        const expr = try p.expectExpression();
        return try p.addNode(.{
            .tag = .phony_assign,
            .main_token = equal_token,
            .lhs = expr,
        });
    } else if (try p.lhsExpression()) |lhs| {
        const op_token = p.advanceToken();
        switch (p.tokens.items(.tag)[op_token]) {
            .@"++" => {
                return try p.addNode(.{
                    .tag = .increase,
                    .main_token = op_token,
                    .lhs = lhs,
                });
            },
            .@"--" => {
                return try p.addNode(.{
                    .tag = .decrease,
                    .main_token = op_token,
                    .lhs = lhs,
                });
            },
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
            => {
                const expr = try p.expectExpression();
                return try p.addNode(.{
                    .tag = .compound_assign,
                    .main_token = op_token,
                    .lhs = lhs,
                    .rhs = expr,
                });
            },
            else => {
                try p.errors.append(p.allocator, Ast.Error{ .tag = .invalid_assignment_op, .token = op_token });
                return Error.Parsing;
            },
        }
    }

    return null;
}

fn whileStatement(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_while) orelse return null;
    const cond = try p.expectExpression();
    const body = try p.expectCompoundStatement(try p.attributeList());
    return try p.addNode(.{
        .tag = .@"while",
        .main_token = main_token,
        .lhs = cond,
        .rhs = body,
    });
}

fn expectTypeSpecifier(p: *Self) Error!Node.Index {
    return try p.typeSpecifier() orelse {
        try p.errors.append(p.allocator, Ast.Error{
            .tag = .expected_type_specifier,
            .token = p.tok_i,
        });
        return Error.Parsing;
    };
}

fn typeSpecifier(p: *Self) Error!?Node.Index {
    if (p.eatToken(.ident)) |t| return try p.addNode(.{ .tag = .ident, .main_token = t });
    return p.typeSpecifierWithoutIdent();
}

fn typeSpecifierWithoutIdent(p: *Self) Error!?Node.Index {
    const main_token = p.advanceToken();
    switch (p.tokens.items(.tag)[main_token]) {
        .k_bool => return try p.addNode(.{ .tag = .bool_type, .main_token = main_token }),
        .k_i32,
        .k_u32,
        .k_f32,
        => return try p.addNode(.{ .tag = .number_type, .main_token = main_token }),
        .k_f16 => {
            if (p.enable_extensions.f16) {
                return try p.addNode(.{ .tag = .number_type, .main_token = main_token });
            }

            try p.errors.append(p.allocator, Ast.Error{
                .tag = .type_needs_ext,
                .token = main_token,
            });
            return Error.Parsing;
        },
        .k_vec2, .k_vec3, .k_vec4 => {
            var elem_type: Node.Index = 0;

            if (p.eatToken(.template_args_start)) |_| {
                elem_type = try p.expectTypeSpecifier();
                _ = try p.expectToken(.template_args_end);
            }

            return try p.addNode(.{
                .tag = .vector_type,
                .main_token = main_token,
                .lhs = elem_type,
            });
        },
        .k_mat2x2,
        .k_mat2x3,
        .k_mat2x4,
        .k_mat3x2,
        .k_mat3x3,
        .k_mat3x4,
        .k_mat4x2,
        .k_mat4x3,
        .k_mat4x4,
        => {
            var elem_type: Node.Index = 0;

            if (p.eatToken(.template_args_start)) |_| {
                elem_type = try p.expectTypeSpecifier();
                _ = try p.expectToken(.template_args_end);
            }

            return try p.addNode(.{
                .tag = .matrix_type,
                .main_token = main_token,
                .lhs = elem_type,
            });
        },
        .k_sampler, .k_sampler_comparison => {
            return try p.addNode(.{ .tag = .sampler_type, .main_token = main_token });
        },
        .k_atomic => {
            _ = try p.expectToken(.template_args_start);
            const elem_type = try p.expectTypeSpecifier();
            _ = try p.expectToken(.template_args_end);
            return try p.addNode(.{
                .tag = .atomic_type,
                .main_token = main_token,
                .lhs = elem_type,
            });
        },
        .k_array => {
            var elem_type: Node.Index = 0;
            var size: Node.Index = 0;

            if (p.eatToken(.template_args_start)) |_| {
                elem_type = try p.expectTypeSpecifier();
                if (p.eatToken(.@",")) |_| {
                    size = try p.elementCountExpr() orelse {
                        try p.errors.append(p.allocator, Ast.Error{
                            .tag = .invalid_element_count,
                            .token = p.tok_i,
                        });
                        return Error.Parsing;
                    };
                }
                _ = try p.expectToken(.template_args_end);
            }

            return try p.addNode(.{
                .tag = .array_type,
                .main_token = main_token,
                .lhs = elem_type,
                .rhs = size,
            });
        },
        .k_ptr => {
            _ = try p.expectToken(.template_args_start);

            const addr_space = try p.expectAddressSpace();
            _ = try p.expectToken(.@",");
            const elem_type = try p.expectTypeSpecifier();
            var access_mode: Token.Index = 0;
            if (p.eatToken(.@",")) |_| {
                access_mode = try p.expectAccessMode();
            }
            _ = try p.expectToken(.template_args_end);

            const extra = try p.addExtra(Node.PtrType{
                .addr_space = addr_space,
                .access_mode = access_mode,
            });
            return try p.addNode(.{
                .tag = .ptr_type,
                .main_token = main_token,
                .lhs = elem_type,
                .rhs = extra,
            });
        },
        .k_texture_1d,
        .k_texture_2d,
        .k_texture_2d_array,
        .k_texture_3d,
        .k_texture_cube,
        .k_texture_cube_array,
        => {
            _ = try p.expectToken(.template_args_start);
            const elem_type = try p.expectTypeSpecifier();
            _ = try p.expectToken(.template_args_end);
            return try p.addNode(.{
                .tag = .sampled_texture_type,
                .main_token = main_token,
                .lhs = elem_type,
            });
        },
        .k_texture_multisampled_2d => {
            _ = try p.expectToken(.template_args_start);
            const elem_type = try p.expectTypeSpecifier();
            _ = try p.expectToken(.template_args_end);
            return try p.addNode(.{
                .tag = .multisampled_texture_type,
                .main_token = main_token,
                .lhs = elem_type,
            });
        },
        .k_texture_depth_multisampled_2d => {
            return try p.addNode(.{
                .tag = .multisampled_texture_type,
                .main_token = main_token,
            });
        },
        .k_texture_external => {
            return try p.addNode(.{
                .tag = .external_texture_type,
                .main_token = main_token,
            });
        },
        .k_texture_depth_2d,
        .k_texture_depth_2d_array,
        .k_texture_depth_cube,
        .k_texture_depth_cube_array,
        => {
            return try p.addNode(.{
                .tag = .depth_texture_type,
                .main_token = main_token,
            });
        },
        .k_texture_storage_1d,
        .k_texture_storage_2d,
        .k_texture_storage_2d_array,
        .k_texture_storage_3d,
        => {
            _ = try p.expectToken(.template_args_start);
            const texel_format = try p.expectTexelFormat();
            _ = try p.expectToken(.@",");
            const access_mode = try p.expectAccessMode();
            _ = try p.expectToken(.template_args_end);
            return try p.addNode(.{
                .tag = .storage_texture_type,
                .main_token = main_token,
                .lhs = texel_format,
                .rhs = access_mode,
            });
        },
        else => return null,
    }
}

fn expectAddressSpace(p: *Self) Error!Token.Index {
    const token = p.advanceToken();
    if (p.tokens.items(.tag)[token] == .ident) {
        const str = p.tokenSource(token);
        if (std.meta.stringToEnum(Node.AddressSpace, str)) |_| {
            return token;
        }
    }

    try p.errors.append(p.allocator, Ast.Error{ .tag = .invalid_address_space, .token = token });
    return Error.Parsing;
}

fn expectAccessMode(p: *Self) Error!Token.Index {
    const token = p.advanceToken();
    if (p.tokens.items(.tag)[token] == .ident) {
        const str = p.tokenSource(token);
        if (std.meta.stringToEnum(Node.AccessMode, str)) |_| {
            return token;
        }
    }

    try p.errors.append(p.allocator, Ast.Error{ .tag = .invalid_access_mode, .token = token });
    return Error.Parsing;
}

fn expectTexelFormat(p: *Self) Error!Token.Index {
    const token = p.advanceToken();
    if (p.tokens.items(.tag)[token] == .ident) {
        const str = p.tokenSource(token);
        if (std.meta.stringToEnum(Node.TexelFormat, str)) |_| {
            return token;
        }
    }

    try p.errors.append(p.allocator, Ast.Error{
        .tag = .invalid_texel_format,
        .token = token,
    });
    return Error.Parsing;
}

/// '(' expression ')'
fn expectParenExpr(p: *Self) Error!Node.Index {
    const main_token = try p.expectToken(.@"(");
    const expr = try p.expectExpression();
    _ = try p.expectToken(.@")");
    return try p.addNode(.{
        .tag = .paren_expr,
        .main_token = main_token,
        .lhs = expr,
    });
}

/// template_elaborated_ident argument_expression_list
fn callExpr(p: *Self) Error!?Node.Index {
    const main_token = p.tok_i;
    var rhs: Node.Index = 0;

    switch (p.peekToken(.tag, 0)) {
        // fn call or struct construct
        .ident => {
            if (p.peekToken(.tag, 1) == .@"(") {
                _ = p.advanceToken();
            } else {
                return null;
            }
        },
        // construct
        .k_bool,
        .k_u32,
        .k_i32,
        .k_f32,
        .k_f16,
        .k_vec2,
        .k_vec3,
        .k_vec4,
        .k_mat2x2,
        .k_mat2x3,
        .k_mat2x4,
        .k_mat3x2,
        .k_mat3x3,
        .k_mat3x4,
        .k_mat4x2,
        .k_mat4x3,
        .k_mat4x4,
        .k_array,
        => {
            rhs = try p.typeSpecifierWithoutIdent() orelse return null;
        },
        else => return null,
    }

    _ = try p.expectToken(.@"(");
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const expr = try p.expression() orelse break;
        try p.scratch.append(p.allocator, expr);
        if (p.eatToken(.@",") == null) break;
    }
    try p.expectAttribEnd();
    const args = p.scratch.items[scratch_top..];

    return try p.addNode(.{
        .tag = .call,
        .main_token = main_token,
        .lhs = try p.listToSpan(args),
        .rhs = rhs,
    });
}

/// | relational_expression
/// | short_circuit_or_expression '||' relational_expression
/// | short_circuit_and_expression '&&' relational_expression
/// | bitwise_expression
fn expression(p: *Self) Error!?Node.Index {
    const lhs_unary = try p.unaryExpr() orelse return null;
    if (try p.bitwiseExpr(lhs_unary)) |bitwise| return bitwise;
    const lhs = try p.expectRelationalExpr(lhs_unary);
    return try p.expectShortCircuitExpr(lhs);
}

fn expectExpression(p: *Self) Error!Node.Index {
    return try p.expression() orelse {
        try p.errors.append(p.allocator, Ast.Error{
            .tag = .expected_expr,
            .token = p.tok_i,
        });
        return Error.Parsing;
    };
}

/// | core_lhs_expression component_or_swizzle_specifier ?
/// | '*' lhs_expression
/// | '&' lhs_expression
///
/// core_lhs_expression :
///  | ident _disambiguate_template
///  | '(' lhs_expression ')'
fn lhsExpression(p: *Self) Error!?Node.Index {
    if (p.eatToken(.ident)) |ident_token| {
        return try p.componentOrSwizzleSpecifier(
            try p.addNode(.{ .tag = .ident, .main_token = ident_token }),
        );
    }

    if (p.eatToken(.@"(")) |_| {
        const expr = try p.expectLhsExpression();
        _ = try p.expectToken(.@")");
        return try p.componentOrSwizzleSpecifier(expr);
    }

    if (p.eatToken(.@"*")) |star_token| {
        return try p.addNode(.{
            .tag = .deref,
            .main_token = star_token,
            .lhs = try p.expectLhsExpression(),
        });
    }

    if (p.eatToken(.@"&")) |addr_of_token| {
        return try p.addNode(.{
            .tag = .addr_of,
            .main_token = addr_of_token,
            .lhs = try p.expectLhsExpression(),
        });
    }

    return null;
}

fn expectLhsExpression(p: *Self) Error!Node.Index {
    return try p.lhsExpression() orelse {
        try p.errors.append(p.allocator, Ast.Error{
            .tag = .expected_lhs_expr,
            .token = p.tok_i,
        });
        return Error.Parsing;
    };
}

fn singularExpr(p: *Self) Error!?Node.Index {
    const prefix = try p.primaryExpr() orelse return null;
    return try p.componentOrSwizzleSpecifier(prefix);
}

/// | template_elaborated_ident
/// | call_expression
/// | literal
/// | paren_expression
fn primaryExpr(p: *Self) Error!?Node.Index {
    if (try p.callExpr()) |call| return call;
    const main_token = p.tok_i;
    switch (p.tokens.items(.tag)[main_token]) {
        .k_true => {
            _ = p.advanceToken();
            return try p.addNode(.{ .tag = .true, .main_token = main_token });
        },
        .k_false => {
            _ = p.advanceToken();
            return try p.addNode(.{ .tag = .false, .main_token = main_token });
        },
        .number => {
            _ = p.advanceToken();
            return try p.addNode(.{ .tag = .number, .main_token = main_token });
        },
        .k_bitcast => {
            _ = p.advanceToken();
            _ = try p.expectToken(.template_args_start);
            const dest_type = try p.expectTypeSpecifier();
            _ = try p.expectToken(.template_args_end);
            const expr = try p.expectParenExpr();
            return try p.addNode(.{
                .tag = .bitcast,
                .main_token = main_token,
                .lhs = dest_type,
                .rhs = expr,
            });
        },
        .@"(" => return try p.expectParenExpr(),
        .ident => {
            _ = p.advanceToken();
            return try p.addNode(.{ .tag = .ident, .main_token = main_token });
        },
        else => {
            return null;
        },
    }
}

fn elementCountExpr(p: *Self) Error!?Node.Index {
    const left = try p.unaryExpr() orelse return null;
    if (try p.bitwiseExpr(left)) |right| return right;
    return try p.expectMathExpr(left);
}

/// | singular_expression
/// | `'-'` unary_expression
/// | `'!'` unary_expression
/// | `'~'` unary_expression
/// | `'*'` unary_expression
/// | `'&'` unary_expression
fn unaryExpr(p: *Self) Error!?Node.Index {
    const op_token = p.tok_i;
    const op: Node.Tag = switch (p.tokens.items(.tag)[op_token]) {
        .@"!", .@"~" => .not,
        .@"-" => .negate,
        .@"*" => .deref,
        .@"&" => .addr_of,
        else => return p.singularExpr(),
    };
    _ = p.advanceToken();

    const expr = try p.expectUnaryExpr();
    return try p.addNode(.{
        .tag = op,
        .main_token = op_token,
        .lhs = expr,
    });
}

fn expectUnaryExpr(p: *Self) Error!Node.Index {
    return try p.unaryExpr() orelse {
        try p.errors.append(p.allocator, Ast.Error{ .tag = .expected_unary_expr, .token = p.tok_i });
        return Error.Parsing;
    };
}

fn expectRelationalExpr(p: *Self, lhs_unary: Node.Index) Error!Node.Index {
    const lhs = try p.expectShiftExpr(lhs_unary);
    const op_token = p.tok_i;
    const op: Node.Tag = switch (p.tokens.items(.tag)[op_token]) {
        .@"==" => .equal,
        .@"!=" => .not_equal,
        .@">" => .greater_than,
        .@">=" => .greater_than_equal,
        .@"<" => .less_than,
        .@"<=" => .less_than_equal,
        else => return lhs,
    };
    _ = p.advanceToken();

    const rhs_unary = try p.expectUnaryExpr();
    const rhs = try p.expectShiftExpr(rhs_unary);
    return try p.addNode(.{
        .tag = op,
        .main_token = op_token,
        .lhs = lhs,
        .rhs = rhs,
    });
}

fn expectShortCircuitExpr(p: *Self, lhs_relational: Node.Index) Error!Node.Index {
    var lhs = lhs_relational;

    const op_token = p.tok_i;
    const op: Node.Tag = switch (p.tokens.items(.tag)[op_token]) {
        .@"&&" => .logical_and,
        .@"||" => .logical_or,
        else => return lhs,
    };

    while (p.peekToken(.tag, 0) == p.tokens.items(.tag)[op_token]) {
        _ = p.advanceToken();

        const rhs_unary = try p.expectUnaryExpr();
        const rhs = try p.expectRelationalExpr(rhs_unary);

        lhs = try p.addNode(.{
            .tag = op,
            .main_token = op_token,
            .lhs = lhs,
            .rhs = rhs,
        });
    }

    return lhs;
}

/// | binary_and_expression `'&'` unary_expression
/// | binary_or_expression `'|'` unary_expression
/// | binary_xor_expression `'^'` unary_expression
fn bitwiseExpr(p: *Self, lhs: Node.Index) Error!?Node.Index {
    const op_token = p.tok_i;
    const op: Node.Tag = switch (p.tokens.items(.tag)[op_token]) {
        .@"&" => .@"and",
        .@"|" => .@"or",
        .@"^" => .xor,
        else => return null,
    };
    _ = p.advanceToken();

    var lhs_result = lhs;
    while (true) {
        const rhs = try p.expectUnaryExpr();

        lhs_result = try p.addNode(.{
            .tag = op,
            .main_token = op_token,
            .lhs = lhs_result,
            .rhs = rhs,
        });

        if (p.peekToken(.tag, 0) != p.tokens.items(.tag)[op_token]) return lhs_result;
    }
}

fn expectShiftExpr(p: *Self, lhs: Node.Index) Error!Node.Index {
    const op_token = p.tok_i;
    const op: Node.Tag = switch (p.tokens.items(.tag)[op_token]) {
        .@"<<" => .shl,
        .@">>" => .shl,
        else => return try p.expectMathExpr(lhs),
    };
    _ = p.advanceToken();

    const rhs = try p.expectUnaryExpr();

    return try p.addNode(.{
        .tag = op,
        .main_token = op_token,
        .lhs = lhs,
        .rhs = rhs,
    });
}

fn expectMathExpr(p: *Self, left: Node.Index) Error!Node.Index {
    const right = try p.expectMultiplicativeExpr(left);
    return p.expectAdditiveExpr(right);
}

fn expectAdditiveExpr(p: *Self, lhs_mul: Node.Index) Error!Node.Index {
    var lhs = lhs_mul;
    while (true) {
        const op_token = p.tok_i;
        const op: Node.Tag = switch (p.tokens.items(.tag)[op_token]) {
            .@"+" => .add,
            .@"-" => .sub,
            else => return lhs,
        };
        _ = p.advanceToken();
        const unary = try p.expectUnaryExpr();
        const rhs = try p.expectMultiplicativeExpr(unary);
        lhs = try p.addNode(.{
            .tag = op,
            .main_token = op_token,
            .lhs = lhs,
            .rhs = rhs,
        });
    }
}

fn expectMultiplicativeExpr(p: *Self, lhs_unary: Node.Index) Error!Node.Index {
    var lhs = lhs_unary;
    while (true) {
        const op_token = p.tok_i;
        const node_tag: Node.Tag = switch (p.peekToken(.tag, 0)) {
            .@"*" => .mul,
            .@"/" => .div,
            .@"%" => .mod,
            else => return lhs,
        };
        _ = p.advanceToken();
        const rhs = try p.expectUnaryExpr();
        lhs = try p.addNode(.{
            .tag = node_tag,
            .main_token = op_token,
            .lhs = lhs,
            .rhs = rhs,
        });
    }
}

fn componentOrSwizzleSpecifier(p: *Self, prefix: Node.Index) Error!Node.Index {
    var prefix_result = prefix;
    while (true) {
        if (p.eatToken(.@".")) |t| {
            const member_token = try p.expectToken(.ident);
            prefix_result = try p.addNode(.{
                .tag = .field_access,
                .main_token = t,
                .lhs = prefix_result,
                .rhs = member_token,
            });
        } else if (p.eatToken(.@"[")) |t| {
            const index_expr = try p.expectExpression();
            _ = try p.expectToken(.@"]");
            prefix_result = try p.addNode(.{
                .tag = .index_access,
                .main_token = t,
                .lhs = prefix_result,
                .rhs = index_expr,
            });
        } else return prefix_result;
    }
}
