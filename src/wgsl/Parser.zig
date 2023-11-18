const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");
const Node = @import("Node.zig");

const Self = @This();
const Allocator = std.mem.Allocator;
const Error = error{ OutOfMemory, Parsing };
const Data = Node.Data;
const max_template_depth = 16;

allocator: Allocator,
// Used to later extract token text
source: [:0]const u8,
// Mutated for template tags
tokens: Ast.TokenList,
// Current parsing position
tok_i: Token.Index = 0,
// Main data structure
nodes: Ast.NodeList = .{},
spans: std.ArrayListUnmanaged(Node.Index) = .{},
extra: std.ArrayListUnmanaged(Node.Index) = .{},
// Used to build lists
scratch: std.ArrayListUnmanaged(Node.Index) = .{},
errors: std.ArrayListUnmanaged(Ast.Error) = .{},

pub fn init(allocator: Allocator, source: [:0]const u8, tokens: Ast.TokenList) !Self {
    var res = Self{
        .allocator = allocator,
        .source = source,
        .tokens = tokens,
    };
    try res.nodes.ensureTotalCapacity(allocator, tokens.len / 2 + 1);
    return res;
}

pub fn deinit(self: *Self, allocator: Allocator) void {
    defer self.errors.deinit(allocator);
    defer self.nodes.deinit(allocator);
    defer self.extra.deinit(allocator);
    defer self.scratch.deinit(allocator);
}

pub const root_token_i = std.math.maxInt(Token.Index);

/// global_directive* global_decl*
pub fn parseTranslationUnit(p: *Self) error{OutOfMemory}!void {
    try p.parameterizeTemplates();
    // Root node must be index 0.
    p.nodes.appendAssumeCapacity(Node{
        .tag = .span,
        .token = root_token_i,
        .data = Data{ .span = .{ .from = 0, .to = 0 } },
    });

    while (p.peekToken(.tag, 0) != .eof) {
        const directive = p.globalDirective() catch |err| switch (err) {
            Error.Parsing => {
                p.findNextGlobalDirective();
                continue;
            },
            error.OutOfMemory => return error.OutOfMemory,
        };
        if (directive) |d| try p.scratch.append(p.allocator, d) else break;
    }

    while (p.peekToken(.tag, 0) != .eof) {
        const decl = p.globalDecl() catch |err| switch (err) {
            Error.Parsing => {
                p.findNextGlobalDecl();
                continue;
            },
            error.OutOfMemory => return error.OutOfMemory,
        };
        if (decl) |d| try p.scratch.append(p.allocator, d);
    }

    try p.spans.appendSlice(p.allocator, p.scratch.items);
    var span = &p.nodes.items(.data)[0].span;
    span.from = @intCast(p.spans.items.len - p.scratch.items.len);
    span.to = @intCast(p.spans.items.len);
}

/// Disambiguate templates (since WGSL chose < and >)
/// https://gpuweb.github.io/gpuweb/wgsl/#template-lists-sec
fn parameterizeTemplates(p: *Self) error{OutOfMemory}!void {
    const UnclosedCandidate = struct {
        token_tag: *Token.Tag,
        depth: u32,
    };
    var discovered_tmpls = std.BoundedArray(UnclosedCandidate, max_template_depth){};
    var depth: u32 = 0;

    var i: u32 = 0;
    while (i < p.tokens.len) : (i += 1) {
        switch (p.tokens.items(.tag)[i]) {
            .ident, .k_var => if (p.tokens.items(.tag)[i + 1] == .@"<") {
                discovered_tmpls.append(.{
                    .token_tag = &p.tokens.items(.tag)[i + 1],
                    .depth = depth,
                }) catch {
                    try p.addError(.deep_template, i);
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

fn listToSpan(
    p: *Self,
    start_tok: Token.Index,
    list: []const Node.Index,
) error{OutOfMemory}!Node.Index {
    if (list.len == 0) return 0;
    try p.spans.appendSlice(p.allocator, list);
    return try p.addNode(start_tok, Data.Span{
        .from = @intCast(p.spans.items.len - list.len),
        .to = @intCast(p.spans.items.len),
    });
}

fn spanToList(self: Self, i: Node.Index) []const Node.Index {
    const span = self.nodes.items(.data)[i].span;
    return self.spans.items[span.from..span.to];
}

fn addNode(p: *Self, token: Token.Index, data_member: anytype) error{OutOfMemory}!Node.Index {
    const i: Node.Index = @intCast(p.nodes.len);
    const field_name: []const u8 = brk: {
        const info = @typeInfo(Data).Union;
        comptime for (info.fields) |f| {
            if (f.type == @TypeOf(data_member)) {
                break :brk f.name;
            }
        };
        std.debug.print("could not find member {any} in {any}\n", .{ data_member, Data });
        unreachable;
    };
    const tag = std.meta.stringToEnum(Node.Tag, field_name).?;
    // If this is hit check that every field in Data has a unique type.
    std.debug.assert(tag != .empty);

    try p.nodes.append(p.allocator, Node{
        .tag = tag,
        .token = token,
        .data = @unionInit(Data, field_name, data_member),
    });
    return i;
}

fn addExtra(p: *Self, extra: anytype) error{OutOfMemory}!Node.Index {
    const fields = std.meta.fields(@TypeOf(extra));
    try p.extra.ensureUnusedCapacity(p.allocator, fields.len);
    const result: Node.Index = @intCast(p.extra.items.len);
    inline for (fields) |field| {
        p.extra.appendAssumeCapacity(@field(extra, field.name));
    }
    return result;
}

fn addError(p: *Self, tag: Ast.Error.Tag, token: ?Token.Index) !void {
    try p.errors.append(p.allocator, Ast.Error{
        .tag = tag,
        .token = token orelse p.tok_i,
    });
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

/// '(' severity_control_name ',' diagnostic_rule_name attrib_end
/// diagnostic_rule_name:
///   | diagnostic_name_token
///   | diagnostic_name_token '.' diagnostic_name_token
fn expectDiagnosticControl(p: *Self) Error!Node.Index {
    _ = try p.expectToken(.@"(");
    const sev_tok = try p.expectToken(.ident);
    _ = std.meta.stringToEnum(Node.Severity, p.tokenSource(sev_tok)) orelse {
        try p.addError(.invalid_severity, sev_tok);
        return Error.Parsing;
    };
    _ = try p.expectToken(.@",");

    var control = Node.DiagnosticControl{
        .severity = sev_tok,
        .name = try p.expectToken(.ident),
    };
    if (p.eatToken(.@".")) |_| control.field = try p.expectToken(.ident);
    try p.expectAttribEnd();

    return try p.addExtra(control);
}

// 'diagnostic' diagnostic_control ';'
fn diagnosticDirective(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_diagnostic) orelse return null;
    const control = try p.expectDiagnosticControl();
    _ = try p.expectToken(.@";");
    return try p.addNode(main_token, Data.DiagnosticDirective{ .control = control });
}

/// | enable_extension_list :
///   enable_extension_name ( ',' enable_extension_name ) * ',' ?
/// | software_extension_list :
///   software_extension_name ( `','` software_extension_name ) * `','` ?
fn identTokenList(p: *Self) Error!Node.Index {
    const span_start = p.tok_i;
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const tok = p.eatKeyword() orelse try p.expectToken(.ident);
        try p.scratch.append(p.allocator, tok);
        if (p.eatToken(.@",") == null) break;
    }
    _ = p.eatToken(.@",");
    return try p.listToSpan(span_start, p.scratch.items[scratch_top..]);
}

/// 'enable' enable_extension_list ';'
fn enableDirective(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_enable) orelse return null;
    const extensions = try p.identTokenList();
    _ = try p.expectToken(.@";");
    return try p.addNode(main_token, Data.EnableDirective{ .ident_tokens = extensions });
}

/// 'requires' software_extension_list ';'
fn requiresDirective(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_requires) orelse return null;
    const extensions = try p.identTokenList();
    _ = try p.expectToken(.@";");
    return try p.addNode(main_token, Data.RequiresDirective{ .ident_tokens = extensions });
}

/// diagnostic_directive | enable_directive | requires_directive
fn globalDirective(p: *Self) Error!?Node.Index {
    return try p.diagnosticDirective() orelse
        try p.enableDirective() orelse
        try p.requiresDirective();
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

/// ;
/// | global_variable_decl ;
/// | override_decl ;
/// | function_decl
/// | const_decl ;
/// | type_alias_decl ;
/// | const_assert_statement ;
/// | struct_decl
fn globalDecl(p: *Self) Error!?Node.Index {
    if (p.eatToken(.@";")) |_| return null; // Skip empty decls

    const attrs = try p.attributeList();
    if (try p.globalVariableDecl(attrs) orelse try p.globalOverrideDecl(attrs)) |node| {
        _ = try p.expectToken(.@";");
        return node;
    }
    if (try p.fnDecl(attrs)) |node| return node;

    if (attrs != 0) {
        try p.addError(.invalid_attributes, p.nodes.items(.token)[p.spanToList(attrs)[0]]);
        return Error.Parsing;
    }
    if (try p.constDecl() orelse
        try p.typeAliasDecl() orelse
        try p.constAssertStatement() orelse
        try p.importDecl()) |node|
    {
        _ = try p.expectToken(.@";");
        return node;
    }

    if (try p.structDecl()) |node| return node;

    try p.addError(.expected_global_decl, null);
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

/// | '@' 'compute'
/// | '@' 'const'
/// | '@' 'fragment'
/// | '@' 'invariant'
/// | '@' 'must_use'
/// | '@' 'vertex'
/// | '@' 'align' '(' expression attrib_end
/// | '@' 'binding' '(' expression attrib_end
/// | '@' 'builtin' '(' expression attrib_end
/// | '@' 'diagnostic' diagnostic_control
/// | '@' 'group' '(' expression attrib_end
/// | '@' 'id' '(' expression attrib_end
/// | '@' 'location' '(' expression attrib_end
/// | '@' 'size' '(' expression attrib_end
/// | '@' 'interpolate' '(' expression attrib_end
/// | '@' 'interpolate' '(' expression ',' expression attrib_end
/// | '@' 'workgroup_size' '(' expression attrib_end
/// | '@' 'workgroup_size' '(' expression ',' expression attrib_end
/// | '@' 'workgroup_size' '(' expression ',' expression ',' expression attrib_end
fn attribute(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.@"@") orelse return null;
    const ident_token = p.eatToken(.k_diagnostic) orelse try p.expectToken(.ident);
    const str = p.tokenSource(ident_token);
    const tag = std.meta.stringToEnum(Node.Attribute.Tag, str) orelse {
        try p.addError(.invalid_attribute, ident_token);
        return Error.Parsing;
    };

    var attr = Data.Attr{ .tag = tag, .data = undefined };

    switch (tag) {
        inline .compute, .@"const", .fragment, .invariant, .must_use, .vertex => |t| {
            attr.data = @unionInit(Node.Attribute, @tagName(t), {});
        },
        inline .@"align", .binding, .builtin, .group, .id, .location, .size => |t| {
            _ = try p.expectToken(.@"(");
            const e = try p.expectExpression();
            try p.expectAttribEnd();
            attr.data = @unionInit(Node.Attribute, @tagName(t), e);
        },
        .diagnostic => attr.data = .{ .diagnostic = try p.expectDiagnosticControl() },
        .interpolate => {
            _ = try p.expectToken(.@"(");
            var interpolation = Node.Interpolation{
                .type = try p.expectExpression(),
            };
            if (p.eatToken(.@",") != null and p.peekToken(.tag, 0) != .@")") {
                interpolation.sampling_expr = try p.expectExpression();
            }
            try p.expectAttribEnd();
            attr.data = .{ .interpolate = try p.addExtra(interpolation) };
        },
        .workgroup_size => {
            _ = try p.expectToken(.@"(");
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
            attr.data = .{ .workgroup_size = try p.addExtra(workgroup_size) };
        },
    }

    return try p.addNode(main_token, attr);
}

/// attribute*
fn attributeList(p: *Self) Error!Node.Index {
    const start_tok = p.tok_i;
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        if (try p.attribute()) |a| try p.scratch.append(p.allocator, a) else break;
    }
    return try p.listToSpan(start_tok, p.scratch.items[scratch_top..]);
}

const Ident = struct {
    name: Token.Index,
    type: Node.Index = 0,
    initializer: Node.Index = 0,
};

/// ident ( ':' type_specifier )?
fn expectOptionallyTypedIdent(p: *Self) Error!Ident {
    var res = Ident{ .name = try p.expectToken(.ident) };
    if (p.eatToken(.@":")) |_| res.type = try p.expectTypeSpecifier();
    return res;
}

/// ident ( ':' type_specifier )? ( = expression )?
/// Type specifier or expression must be included.
fn expectOptionallyTypedIdentWithInitializer(p: *Self) Error!Ident {
    var res = try p.expectOptionallyTypedIdent();
    if (p.eatToken(.@"=")) |_| res.initializer = try p.expectExpression();
    if (res.initializer == 0 and res.type == 0) {
        try p.addError(.invalid_initializer, res.name);
        return Error.Parsing;
    }

    return res;
}

/// attribute* 'var' template_list? optionally_typed_ident ( '=' expression )?
fn globalVariableDecl(p: *Self, attrs: Node.Index) Error!?Node.Index {
    const main_token = p.eatToken(.k_var) orelse return null;
    const template_list = try p.templateList();
    const ident = try p.expectOptionallyTypedIdentWithInitializer();
    const extra = try p.addExtra(Node.GlobalVar{
        .attrs = attrs,
        .name = ident.name,
        .template_list = template_list,
        .type = ident.type,
    });
    return try p.addNode(main_token, Data.GlobalVar{
        .global_var = extra,
        .initializer = ident.initializer,
    });
}

/// attribute * 'override' optionally_typed_ident ( '=' expression ) ?
fn globalOverrideDecl(p: *Self, attrs: Node.Index) Error!?Node.Index {
    const main_token = p.eatToken(.k_override) orelse return null;
    const ident = try p.expectOptionallyTypedIdentWithInitializer();
    const extra = try p.addExtra(Node.Override{
        .attrs = attrs,
        .type = ident.type,
    });
    return try p.addNode(main_token, Data.Override{
        .override = extra,
        .initializer = ident.initializer,
    });
}

/// 'alias' ident '=' type_specifier
fn typeAliasDecl(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_alias) orelse return null;
    _ = try p.expectToken(.ident);
    _ = try p.expectToken(.@"=");
    const value = try p.expectTypeSpecifier();
    return try p.addNode(main_token, Data.TypeAlias{ .value = value });
}

/// attribute* member_ident ':' type_specifier
fn structMember(p: *Self, attrs: Node.Index) Error!?Node.Index {
    const main_token = p.eatToken(.ident) orelse return null;
    _ = try p.expectToken(.@":");
    const member_type = try p.expectTypeSpecifier();
    return try p.addNode(main_token, Data.StructMember{
        .attributes = attrs,
        .type = member_type,
    });
}

/// 'struct' ident '{' struct_member ( ',' struct_member ) * ',' ? '}'
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
                try p.addError(.expected_struct_member, null);
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
        try p.addError(.empty_struct, name_token);
        return Error.Parsing;
    }
    const start_tok = p.nodes.items(.token)[members[0]];

    return try p.addNode(main_token, Data.Struct{ .members = try p.listToSpan(start_tok, members) });
}

/// 'const_assert' expression
fn constAssertStatement(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_const_assert) orelse return null;
    const expr = try p.expectExpression();
    return try p.addNode(main_token, Data.ConstAssert{ .expr = expr });
}

/// ident '(' param_list ? ')' ( '->' attribute * template_elaborated_ident ) ?
fn fnHeader(p: *Self, attrs: Node.Index) Error!Node.Index {
    _ = try p.expectToken(.ident);
    const start_tok = try p.expectToken(.@"(");
    const params = try p.fnParameterList(start_tok);
    _ = try p.expectToken(.@")");

    var proto = Node.FnHeader{
        .attrs = attrs,
        .params = params,
    };
    if (p.eatToken(.@"->")) |_| {
        proto.return_attrs = try p.attributeList();
        proto.return_type = try p.expectTypeSpecifier();
    }
    return try p.addExtra(proto);
}

/// attribute* 'fn' function_header compound_statement
fn fnDecl(p: *Self, attrs: Node.Index) Error!?Node.Index {
    const main_token = p.eatToken(.k_fn) orelse return null;
    const fn_header = try p.fnHeader(attrs);
    const body = try p.expectCompoundStatement(try p.attributeList());
    return try p.addNode(main_token, Data.Fn{ .fn_header = fn_header, .body = body });
}

// ident ('as' ident)?
fn importAlias(p: *Self) Error!?Node.Index {
    const main_token = try p.expectToken(.ident);
    const alias = if (p.eatToken(.k_as)) |_| try p.expectToken(.ident) else 0;
    return try p.addNode(main_token, Data.Alias{ .alias = alias });
}

// '{' import_alias (',' import_alias)* ','? '}'
fn importList(p: *Self) Error!Node.Index {
    const start_tok = p.eatToken(.@"{") orelse return 0;
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        if (try p.importAlias()) |a| try p.scratch.append(p.allocator, a);
        if (p.eatToken(.@",") == null) break;
    }
    _ = try p.expectToken(.@"}");
    const imports = p.scratch.items[scratch_top..];
    return try p.listToSpan(start_tok, imports);
}

/// 'import' (import_list 'from')? string_literal ';'?
fn importDecl(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_import) orelse return null;
    const imports = try p.importList();
    if (imports != 0) _ = try p.expectToken(.k_from);
    const mod_token = try p.expectToken(.string_literal);

    return try p.addNode(main_token, Data.Import{
        .aliases = imports,
        .module = mod_token,
    });
}

/// attribute* ident ':' type_specifier
fn fnParameter(p: *Self) Error!?Node.Index {
    const attrs = try p.attributeList();
    const main_token = p.eatToken(.ident) orelse {
        if (attrs != 0) {
            try p.addError(.expected_function_parameter, null);
            return Error.Parsing;
        }
        return null;
    };
    _ = try p.expectToken(.@":");
    const param_type = try p.expectTypeSpecifier();
    return try p.addNode(main_token, Data.FnParam{ .attributes = attrs, .type = param_type });
}

/// param ( ',' param )* ','?
fn fnParameterList(p: *Self, start_tok: Token.Index) Error!Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        if (try p.fnParameter()) |pa| try p.scratch.append(p.allocator, pa);
        if (p.eatToken(.@",") == null) break;
    }
    return try p.listToSpan(start_tok, p.scratch.items[scratch_top..]);
}

/// | ';'
/// | loop_statement
/// | compound_statement
/// | for_statement
/// | if_statement
/// | switch_statement
/// | while_statement
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
        try p.compoundStatement(attrs) orelse
        try p.forStatement(attrs)) |node| return node;

    if (attrs != 0) {
        try p.addError(.invalid_attributes, p.nodes.items(.token)[p.spanToList(attrs)[0]]);
        return Error.Parsing;
    }

    if (try p.ifStatement() orelse
        try p.switchStatement() orelse
        try p.whileStatement()) |node| return node;
    if (try p.returnStatement() orelse
        try p.callPhrase() orelse
        try p.variableOrValueStatement() orelse
        try p.breakStatement() orelse
        try p.continueStatement() orelse
        try p.discardStatement() orelse
        try p.variableUpdatingStatement() orelse
        try p.constAssertStatement()) |node|
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
    return try p.variableDecl() orelse try p.letDecl() orelse try p.constDecl();
}

/// 'return' expression ?
fn returnStatement(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_return) orelse return null;
    const expr = try p.expression() orelse 0;
    return try p.addNode(main_token, Data.Return{ .expr = expr });
}

/// attribute* if_clause else_if_clause* else_clause?
/// if_clause :
///  'if' expression compound_statement
/// else_if_clause :
///  'else' 'if' expression compound_statement
/// else_clause :
///  'else' compound_statement
fn ifStatement(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_if) orelse return null;

    const condition = try p.expectExpression();
    const body = try p.expectCompoundStatement(try p.attributeList());
    const if_node = try p.addNode(main_token, Data.If{ .condition = condition, .body = body });

    if (p.eatToken(.k_else)) |else_token| {
        if (p.peekToken(.tag, 0) == .k_if) {
            // Instead of building a list of "else if" clauses, use recursion.
            const else_if = try p.ifStatement() orelse unreachable;
            return try p.addNode(else_token, Data.ElseIf{
                .if1 = if_node,
                .if2 = else_if,
            });
        } else {
            const else_body = try p.expectCompoundStatement(try p.attributeList());
            return try p.addNode(else_token, Data.Else{
                .@"if" = if_node,
                .body = else_body,
            });
        }
    }

    return if_node;
}

/// attribute* '{' statement* last_statement? '}'
fn compoundStatementExtra(p: *Self, attrs: Node.Index, last_statement: anytype) Error!?Node.Index {
    const main_token = p.eatToken(.@"{") orelse return null;
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        if (try p.statement()) |s| try p.scratch.append(p.allocator, s) else break;
    }
    if (try last_statement(p)) |s| try p.scratch.append(p.allocator, s);
    _ = try p.expectToken(.@"}");
    return try p.addNode(main_token, Data.Compound{
        .attributes = attrs,
        .statements = try p.listToSpan(main_token, p.scratch.items[scratch_top..]),
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
        try p.addError(.expected_compound_statement, null);
        return Error.Parsing;
    };
}

/// 'break'
fn breakStatement(p: *Self) Error!?Node.Index {
    if (p.peekToken(.tag, 0) == .k_break and p.peekToken(.tag, 1) != .k_if) {
        return try p.addNode(p.advanceToken(), Data.Break);
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
        return try p.addNode(main_token, Data.BreakIf{ .condition = cond });
    }
    return null;
}

/// 'continue'
fn continueStatement(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_continue) orelse return null;
    return try p.addNode(main_token, Data.Continue{});
}

/// attribute * '{' statement * break_if_statement ? '}'
fn continuingCompoundStatement(p: *Self) Error!?Node.Index {
    return p.compoundStatementExtra(try p.attributeList(), breakIfStatement);
}

fn expectContinuingCompoundStatement(p: *Self) Error!Node.Index {
    return try p.continuingCompoundStatement() orelse {
        try p.addError(.expected_continuing_compound_statement, null);
        return Error.Parsing;
    };
}

/// 'continuing' continuing_compound_statement
fn continuingStatement(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_continuing) orelse return null;
    const body = try p.expectContinuingCompoundStatement();
    return try p.addNode(main_token, Data.Continuing{ .body = body });
}

fn discardStatement(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_discard) orelse return null;
    return try p.addNode(main_token, Data.Discard);
}

/// | variable_or_value_statement
/// | variable_updating_statement
/// | func_call_statement
fn forInit(p: *Self) Error!?Node.Index {
    return try p.variableOrValueStatement() orelse try p.callPhrase() orelse try p.variableUpdatingStatement();
}

/// | variable_updating_statement
/// | func_call_statement
fn forUpdate(p: *Self) Error!?Node.Index {
    return try p.variableUpdatingStatement() orelse try p.callPhrase() orelse 0;
}

/// for_init ? ';' expression ? ';' for_update ?
fn expectForHeader(p: *Self, attrs: Node.Index) Error!Node.Index {
    const for_init = try p.forInit() orelse 0;
    _ = try p.expectToken(.@";");
    const for_cond = try p.expression() orelse 0;
    _ = try p.expectToken(.@";");
    const for_update = try p.forUpdate() orelse 0;
    return try p.addExtra(Node.ForHeader{
        .attrs = attrs,
        .init = for_init,
        .cond = for_cond,
        .update = for_update,
    });
}

/// attribute* 'for' '(' for_header ')' compound_statement
fn forStatement(p: *Self, attrs: Node.Index) Error!?Node.Index {
    const main_token = p.eatToken(.k_for) orelse return null;
    _ = try p.expectToken(.@"(");
    const header = try p.expectForHeader(attrs);
    _ = try p.expectToken(.@")");
    const body = try p.expectCompoundStatement(try p.attributeList());

    return try p.addNode(main_token, Data.For{ .for_header = header, .body = body });
}

// attribute* 'loop' attribute* '{' statement * continuing_statement ? '}'
fn loopStatement(p: *Self, attrs: Node.Index) Error!?Node.Index {
    const main_token = p.eatToken(.k_loop) orelse return null;
    const body = try p.compoundStatementExtra(try p.attributeList(), continuingStatement) orelse {
        try p.addError(.expected_compound_statement, null);
        return Error.Parsing;
    };
    return try p.addNode(main_token, Data.Loop{ .attributes = attrs, .body = body });
}

/// 'default' | expression
fn caseSelector(p: *Self) Error!?Node.Index {
    if (p.eatToken(.k_default)) |t| {
        return try p.addNode(t, Data.CaseSelector{ .expression = 0 });
    } else {
        const main_token = p.tok_i;
        if (try p.expression()) |e| {
            return try p.addNode(main_token, Data.CaseSelector{ .expression = e });
        }
    }
    return null;
}

/// 'case' case_selectors ':' ? compound_statement
/// case_selectors :
///   case_selector ( ',' case_selector ) * ',' ?
fn caseClause(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_case) orelse return null;
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    const case_selector = try p.caseSelector() orelse {
        try p.addError(.expected_case_selector, null);
        return Error.Parsing;
    };
    try p.scratch.append(p.allocator, case_selector);
    while (p.eatToken(.@",") != null) {
        if (try p.caseSelector()) |c| try p.scratch.append(p.allocator, c) else break;
    }
    _ = p.eatToken(.@":");
    return try p.addNode(main_token, Data.CaseClause{
        .selectors = try p.listToSpan(main_token, p.scratch.items[scratch_top..]),
        .body = try p.expectCompoundStatement(try p.attributeList()),
    });
}

/// 'default' ':'? compound_statement
fn defaultAloneClause(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_default) orelse return null;
    _ = p.eatToken(.@":");
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    try p.scratch.append(p.allocator, 0);
    return try p.addNode(main_token, Data.CaseClause{
        .selectors = try p.listToSpan(main_token, p.scratch.items[scratch_top..]),
        .body = try p.expectCompoundStatement(try p.attributeList()),
    });
}

/// case_clause | default_alone_clause
fn switchClause(p: *Self) Error!?Node.Index {
    return try p.caseClause() orelse try p.defaultAloneClause();
}

/// attribute* '{' switch_clause + '}'
fn switchBody(p: *Self) Error!Node.Index {
    const attrs = try p.attributeList();
    const main_token = try p.expectToken(.@"{");
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        if (try p.switchClause()) |c| try p.scratch.append(p.allocator, c) else break;
    }
    _ = try p.expectToken(.@"}");

    return try p.addNode(main_token, Data.SwitchBody{
        .attributes = attrs,
        .clauses = try p.listToSpan(main_token, p.scratch.items[scratch_top..]),
    });
}

/// attribute* 'switch' expression switch_body
fn switchStatement(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_switch) orelse return null;
    const expr = try p.expectExpression();
    const body = try p.switchBody();
    return try p.addNode(main_token, Data.Switch{ .expr = expr, .body = body });
}

// | 'var' template_list? optionally_typed_ident
// | 'var' template_list? optionally_typed_ident '=' expression
fn variableDecl(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_var) orelse return null;
    const template_list = try p.templateList();
    const ident = try p.expectOptionallyTypedIdentWithInitializer();
    const extra = try p.addExtra(Node.Var{
        .name = ident.name,
        .template_list = template_list,
        .type = ident.type,
    });
    return try p.addNode(main_token, Data.Var{
        .@"var" = extra,
        .initializer = ident.initializer,
    });
}

/// 'let' optionally_typed_ident '=' expression
fn letDecl(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_let) orelse return null;
    const ident = try p.expectOptionallyTypedIdentWithInitializer();
    return try p.addNode(main_token, Data.Let{
        .type = ident.type,
        .initializer = ident.initializer,
    });
}

/// 'const' optionally_typed_ident '=' expression
fn constDecl(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.k_const) orelse return null;
    const ident = try p.expectOptionallyTypedIdentWithInitializer();
    return try p.addNode(main_token, Data.Const{
        .type = ident.type,
        .initializer = ident.initializer,
    });
}

/// | '_' '=' expression
/// | lhs_expression ( '=' | compound_assignment_operator ) expression
/// | lhs_expression '++'
/// | lhs_expression '--'
fn variableUpdatingStatement(p: *Self) Error!?Node.Index {
    if (p.eatToken(._)) |_| {
        const main_token = try p.expectToken(.@"=");
        return try p.addNode(main_token, Data.VariableUpdating{
            .lhs_expr = 0,
            .rhs_expr = try p.expectExpression(),
        });
    } else if (try p.lhsExpression()) |lhs| {
        const main_token = p.advanceToken();
        switch (p.tokens.items(.tag)[main_token]) {
            .@"++", .@"--" => {},
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
            => return try p.addNode(main_token, Data.VariableUpdating{
                .lhs_expr = lhs,
                .rhs_expr = try p.expectExpression(),
            }),
            else => {
                try p.addError(.invalid_assignment_op, main_token);
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
    return try p.addNode(main_token, Data.While{
        .condition = cond,
        .body = body,
    });
}

/// template_elaborated_ident
fn typeSpecifier(p: *Self) Error!?Node.Index {
    if (try p.templateElaboratedIdent()) |n| {
        p.nodes.items(.data)[n] = .{ .type = Data.Type{ .template_list = p.nodes.items(.data)[n].ident.template_list } };
        p.nodes.items(.tag)[n] = .type;
        return n;
    }
    return null;
}

fn expectTypeSpecifier(p: *Self) Error!Node.Index {
    return try p.typeSpecifier() orelse {
        try p.addError(.expected_type_specifier, null);
        return Error.Parsing;
    };
}

/// '(' expression ')'
fn parenExpr(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.@"(") orelse return null;
    const expr = try p.expectExpression();
    _ = try p.expectToken(.@")");
    return try p.addNode(main_token, Data.ParenExpr{ .expr = expr });
}

/// expression ( ',' expression ) * ',' ?
fn expressionCommaList(p: *Self, start_tok: Token.Index) Error!Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const expr = try p.expression() orelse break;
        try p.scratch.append(p.allocator, expr);
        if (p.eatToken(.@",") == null) break;
    }
    return try p.listToSpan(start_tok, p.scratch.items[scratch_top..]);
}

/// '(' expression_comma_list ? ')'
fn expectArgumentExpressionList(p: *Self) Error!Node.Index {
    const start_tok = try p.expectToken(.@"(");
    const res = try p.expressionCommaList(start_tok);
    try p.expectAttribEnd();
    return res;
}

/// template_elaborated_ident argument_expression_list
fn callPhrase(p: *Self) Error!?Node.Index {
    const main_token = p.tok_i;
    const ident = try p.templateElaboratedIdent() orelse return null;
    // Unfortunately need lookbehind since sometimes other `templateElaboratedIdent` rules follow.
    if (p.peekToken(.tag, 0) != .@"(") {
        p.tok_i = main_token;
        return null;
    }
    return try p.addNode(main_token, Data.Call{
        .ident = ident,
        .arguments = try p.expectArgumentExpressionList(),
    });
}

/// | relational_expression
/// | short_circuit_or_expression '||' relational_expression
/// | short_circuit_and_expression '&&' relational_expression
/// | bitwise_expression
fn expression(p: *Self) Error!?Node.Index {
    const lhs_unary = try p.unaryExpr() orelse return null;
    if (try p.bitwiseExpr(lhs_unary)) |bitwise| return bitwise;
    const lhs = try p.relationalExpr(lhs_unary);
    return try p.shortCircuitExpr(lhs);
}

fn expectExpression(p: *Self) Error!Node.Index {
    return try p.expression() orelse {
        try p.addError(.expected_expr, null);
        return Error.Parsing;
    };
}

/// | ident
/// | '(' lhs_expression ')'
fn coreLhsExpression(p: *Self) Error!?Node.Index {
    if (p.eatToken(.ident)) |tok| {
        return try p.addNode(tok, Data.Ident{ .template_list = 0 });
    }
    if (p.eatToken(.@"(")) |_| {
        const expr = try p.expectLhsExpression();
        _ = try p.expectToken(.@")");
        return expr;
    }
    return null;
}

/// | core_lhs_expression component_or_swizzle_specifier?
/// | '*' lhs_expression
/// | '&' lhs_expression
fn lhsExpression(p: *Self) Error!?Node.Index {
    if (try p.coreLhsExpression()) |e| return try p.componentOrSwizzle(e);

    switch (p.peekToken(.tag, 0)) {
        .@"*", .@"&" => {
            const main_token = p.advanceToken();
            return try p.addNode(main_token, .{ .lhs_expr = try p.expectLhsExpression() });
        },
        else => return null,
    }
}

fn expectLhsExpression(p: *Self) Error!Node.Index {
    return try p.lhsExpression() orelse {
        try p.addError(.expected_lhs_expr, null);
        return Error.Parsing;
    };
}

/// primary_expression component_or_swizzle_specifier?
fn singularExpr(p: *Self) Error!?Node.Index {
    const prefix = try p.primaryExpr() orelse return null;
    return try p.componentOrSwizzle(prefix);
}

/// | literal
/// | paren_expression
/// | call_expression
/// | template_elaborated_ident
fn primaryExpr(p: *Self) Error!?Node.Index {
    return try p.literal() orelse
        try p.parenExpr() orelse
        try p.callPhrase() orelse
        try p.templateElaboratedIdent();
}

/// '<' expression ( ',' expression ) * ',' ? '>'
fn templateList(p: *Self) Error!Node.Index {
    const start_tok = p.eatToken(.template_args_start) orelse return 0;
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    const expr = try p.expectExpression();
    try p.scratch.append(p.allocator, expr);
    while (p.eatToken(.@",") != null) {
        if (try p.expression()) |e| try p.scratch.append(p.allocator, e) else break;
    }
    _ = try p.expectToken(.template_args_end);
    return try p.listToSpan(start_tok, p.scratch.items[scratch_top..]);
}

/// ident template_list ?
fn templateElaboratedIdent(p: *Self) Error!?Node.Index {
    const main_token = p.eatToken(.ident) orelse return null;
    return try p.addNode(main_token, Data.Ident{ .template_list = try p.templateList() });
}

/// | int_literal
/// | float_literal
/// | bool_literal
fn literal(p: *Self) Error!?Node.Index {
    if (p.eatToken(.k_true)) |t| return try p.addNode(t, Data.True{});
    if (p.eatToken(.k_false)) |t| return try p.addNode(t, Data.False{});
    if (p.eatToken(.number)) |t| return try p.addNode(t, Data.Number{});
    return null;
}

/// | singular_expression
/// | `'!'` unary_expression
/// | `'~'` unary_expression
/// | `'-'` unary_expression
/// | `'*'` unary_expression
/// | `'&'` unary_expression
fn unaryExpr(p: *Self) Error!?Node.Index {
    switch (p.peekToken(.tag, 0)) {
        .@"!", .@"~", .@"-", .@"*", .@"&" => {},
        else => return p.singularExpr(),
    }
    const main_token = p.advanceToken();
    return try p.addNode(main_token, Data.UnaryExpr{ .expr = try p.expectUnaryExpr() });
}

fn expectUnaryExpr(p: *Self) Error!Node.Index {
    return try p.unaryExpr() orelse {
        try p.addError(.expected_unary_expr, null);
        return Error.Parsing;
    };
}

/// | shift_expression
/// | shift_expression _less_than shift_expression
/// | shift_expression _greater_than shift_expression
/// | shift_expression _less_than_equal shift_expression
/// | shift_expression _greater_than_equal shift_expression
/// | shift_expression '==' shift_expression
/// | shift_expression '!=' shift_expression
fn relationalExpr(p: *Self, lhs_unary: Node.Index) Error!Node.Index {
    const lhs = try p.shiftExpr(lhs_unary);
    switch (p.peekToken(.tag, 0)) {
        .@"<", .@">", .@"<=", .@">=", .@"==", .@"!=" => {},
        else => return lhs,
    }
    const main_token = p.advanceToken();
    const rhs = try p.shiftExpr(try p.expectUnaryExpr());
    return try p.addNode(main_token, Data.RelationalExpr{
        .lhs_shift_expr = lhs,
        .rhs_shift_expr = rhs,
    });
}

/// | relational_expression `'||'` relational_expression
/// | relational_expression `'&&'` relational_expression
fn shortCircuitExpr(p: *Self, lhs_relational: Node.Index) Error!Node.Index {
    const op_token = p.tok_i;
    switch (p.tokens.items(.tag)[op_token]) {
        .@"&&", .@"||" => {},
        else => return lhs_relational,
    }

    var lhs = lhs_relational;
    while (p.peekToken(.tag, 0) == p.tokens.items(.tag)[op_token]) {
        _ = p.advanceToken();

        const rhs_unary = try p.expectUnaryExpr();
        const rhs = try p.relationalExpr(rhs_unary);

        lhs = try p.addNode(op_token, Data.ShortCircuitExpr{
            .lhs_relational_expr = lhs,
            .rhs_relational_expr = rhs,
        });
    }

    return lhs;
}

/// | binary_and_expression `'&'` unary_expression
/// | binary_or_expression `'|'` unary_expression
/// | binary_xor_expression `'^'` unary_expression
fn bitwiseExpr(p: *Self, lhs: Node.Index) Error!?Node.Index {
    switch (p.peekToken(.tag, 0)) {
        .@"&", .@"|", .@"^" => {},
        else => return null,
    }
    const main_token = p.advanceToken();

    var lhs_result = lhs;
    while (true) {
        const rhs = try p.expectUnaryExpr();
        lhs_result = try p.addNode(main_token, Data.BitwiseExpr{
            .lhs_bitwise_expr = lhs_result,
            .rhs_unary_expr = rhs,
        });

        if (p.peekToken(.tag, 0) != p.tokens.items(.tag)[main_token]) return lhs_result;
    }
}

/// | additive_expression
/// | unary_expression _shift_left unary_expression
/// | unary_expression _shift_right unary_expression
fn shiftExpr(p: *Self, lhs: Node.Index) Error!Node.Index {
    switch (p.peekToken(.tag, 0)) {
        .@"<<", .@">>" => {},
        else => return try p.mathExpr(lhs),
    }
    const main_token = p.advanceToken();
    return try p.addNode(main_token, Data.ShiftExpr{
        .lhs_unary_expr = lhs,
        .rhs_unary_expr = try p.expectUnaryExpr(),
    });
}

fn mathExpr(p: *Self, left: Node.Index) Error!Node.Index {
    const right = try p.multiplicativeExpr(left);
    return p.additiveExpr(right);
}

/// | multiplicative_expression
/// | additive_expression '+' multiplicative_expression
/// | additive_expression '-' multiplicative_expression
fn additiveExpr(p: *Self, lhs_mul: Node.Index) Error!Node.Index {
    var lhs = lhs_mul;
    while (true) {
        switch (p.peekToken(.tag, 0)) {
            .@"+", .@"-" => {},
            else => return lhs,
        }
        const main_token = p.advanceToken();
        const unary = try p.expectUnaryExpr();
        const rhs = try p.multiplicativeExpr(unary);
        lhs = try p.addNode(main_token, Data.AdditiveExpr{
            .lhs_additive_expr = lhs,
            .rhs_mul_expr = rhs,
        });
    }
}

/// | unary_expression
/// | multiplicative_expression '*' unary_expression
/// | multiplicative_expression '/' unary_expression
/// | multiplicative_expression '%' unary_expression
fn multiplicativeExpr(p: *Self, lhs_unary: Node.Index) Error!Node.Index {
    var lhs = lhs_unary;
    while (true) {
        switch (p.peekToken(.tag, 0)) {
            .@"*", .@"/", .@"%" => {},
            else => return lhs,
        }
        const main_token = p.advanceToken();
        lhs = try p.addNode(main_token, Data.MultiplicativeExpr{
            .lhs_multiplicative_expr = lhs,
            .rhs_unary_expr = try p.expectUnaryExpr(),
        });
    }
}

/// | '[' expression ']' component_or_swizzle_specifier ?
/// | '.' member_ident component_or_swizzle_specifier ?
/// | '.' swizzle_name component_or_swizzle_specifier ?
fn componentOrSwizzle(p: *Self, prefix: Node.Index) Error!Node.Index {
    var prefix_result = prefix;
    while (true) {
        if (p.eatToken(.@".")) |t| {
            prefix_result = try p.addNode(t, Data.FieldAccess{
                .lhs_expr = prefix_result,
                .member = try p.expectToken(.ident),
            });
        } else if (p.eatToken(.@"[")) |t| {
            const index_expr = try p.expectExpression();
            _ = try p.expectToken(.@"]");
            prefix_result = try p.addNode(t, Data.IndexAccess{
                .lhs_expr = prefix_result,
                .index_expr = index_expr,
            });
        } else return prefix_result;
    }
}
