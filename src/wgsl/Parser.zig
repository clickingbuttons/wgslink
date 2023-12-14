const std = @import("std");
const Ast = @import("../ast/Ast.zig");
const Token = @import("./Token.zig");
const Node = @import("../ast/Node.zig");
const AstBuilder = @import("../ast/Builder.zig");
const ParsingError = @import("./ParsingError.zig");
const Tokenizer = @import("./Tokenizer.zig");
const File = @import("../file/File.zig");
const parseNumber = @import("./number.zig").parse;

const Loc = File.Loc;
const Self = @This();
const Allocator = std.mem.Allocator;
const Error = Allocator.Error || error{Parsing};
pub const TokenList = std.MultiArrayList(Token);
pub const max_template_depth = 16;
pub const TokenIndex = Loc.Index;

allocator: Allocator,
/// The main event
builder: AstBuilder,
/// Used for building errors.
path: ?[]const u8,
/// Used to check validity of some idents (like severity and attributes) and
/// write into identifier map.
source: [:0]const u8,
/// Need to store all of these to mutate for template tags.
tokens: TokenList = .{},
/// Current parsing position
tok_i: TokenIndex = 0,
/// Used to build lists
scratch: std.ArrayListUnmanaged(Node.Index) = .{},

pub fn init(allocator: Allocator, path: ?[]const u8, source: [:0]const u8) Allocator.Error!Self {
    var parser = Self{
        .allocator = allocator,
        .builder = try AstBuilder.init(allocator),
        .path = path,
        .source = source,
    };

    try parser.tokens.ensureTotalCapacity(allocator, source.len / 8);
    var tokenizer = Tokenizer.init(source);
    while (true) {
        const token = tokenizer.next();
        switch (token.tag) {
            .@"\n" => try parser.builder.newlines.append(allocator, token.loc.start),
            else => try parser.tokens.append(allocator, token),
        }
        if (token.tag == .eof) break;
    }
    try parser.builder.nodes.ensureTotalCapacity(allocator, parser.tokens.len / 2 + 1);

    return parser;
}

pub fn deinit(self: *Self) void {
    const allocator = self.allocator;
    self.tokens.deinit(allocator);
    self.builder.deinit(allocator);
    self.scratch.deinit(allocator);
}

pub fn parse(allocator: Allocator, path: ?[]const u8, source: [:0]const u8) Allocator.Error!Ast {
    var parser = try init(allocator, path, source);
    defer parser.deinit();
    try parser.parseTranslationUnit();

    return parser.builder.toOwnedAst(allocator);
}

fn listToSpan(
    p: *Self,
    start_token: TokenIndex,
    list: []const Node.Index,
) Allocator.Error!Node.Index {
    const src_offset = p.tokens.items(.loc)[start_token].start;
    return try p.builder.listToSpan(p.allocator, src_offset, list);
}

fn addNodeAdvanced(
    p: *Self,
    src_offset: Loc.Index,
    tag: Node.Tag,
    lhs: Node.Index,
    rhs: Node.Index,
) Allocator.Error!Node.Index {
    return try p.builder.addNode(p.allocator, Node{
        .src_offset = src_offset,
        .tag = tag,
        .lhs = lhs,
        .rhs = rhs,
    });
}

fn addNode(
    p: *Self,
    main_token: TokenIndex,
    tag: Node.Tag,
    lhs: Node.Index,
    rhs: Node.Index,
) Allocator.Error!Node.Index {
    const src_offset = p.tokens.items(.loc)[main_token].start;
    return p.addNodeAdvanced(src_offset, tag, lhs, rhs);
}

fn addExtra(p: *Self, extra: anytype) Allocator.Error!Node.Index {
    return try p.builder.addExtra(p.allocator, extra);
}

pub fn renderErrors(
    self: *Self,
    writer: anytype,
    term: std.io.tty.Config,
    file_path: ?[]const u8,
) !void {
    for (self.errors.items) |e| try e.render(
        self.source,
        self.tokens.items(.loc)[e.token],
        writer,
        term,
        file_path,
    );
}

/// global_directive* global_decl*
fn parseTranslationUnit(p: *Self) Allocator.Error!void {
    try p.parameterizeTemplates();

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

    try p.builder.extra.appendSlice(p.allocator, p.scratch.items);
    p.builder.finishRootSpan(p.scratch.items.len);
}

/// Disambiguate templates (since WGSL chose < and >)
/// https://gpuweb.github.io/gpuweb/wgsl/#template-lists-sec
fn parameterizeTemplates(p: *Self) Allocator.Error!void {
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
                discovered_tmpls.len = 0;
            },
            .@"||", .@"&&" => {
                while (discovered_tmpls.len > 0 and
                    discovered_tmpls.get(discovered_tmpls.len - 1).depth == depth) _ = discovered_tmpls.pop();
            },
            else => {},
        }
    }
}

fn addErrorAdvanced(
    p: *Self,
    tag: ParsingError.Tag,
    token: ?TokenIndex,
    loc_offset: Loc.Index,
    extra: Loc.Index,
) !void {
    const tok = token orelse p.tok_i;
    const tok_loc = p.tokens.items(.loc)[tok];
    const loc = File.Error.ErrorLoc.init(p.builder.newlines.items, tok_loc.start + loc_offset, tok_loc.end);
    const err = File.Error{
        .loc = loc,
        .path = p.path,
        .source = p.source,
        .data = .{ .wgsl = .{
            .tag = tag,
            .extra = extra,
        } },
    };
    try p.builder.errors.append(p.allocator, err);
}

fn addError(p: *Self, tag: ParsingError.Tag, token: ?TokenIndex) !void {
    try p.addErrorAdvanced(tag, token, 0, 0);
}

fn peekToken(
    p: Self,
    comptime field: TokenList.Field,
    offset: usize,
) std.meta.fieldInfo(Token, field).type {
    return p.tokens.items(field)[p.tok_i + offset];
}

fn advanceToken(p: *Self) TokenIndex {
    const prev = p.tok_i;
    p.tok_i = @min(prev + 1, p.tokens.len - 1);
    return prev;
}

fn isKeyword(tag: Token.Tag) bool {
    const tag_name = @tagName(tag);
    return tag_name.len > 2 and std.mem.eql(u8, tag_name[0..2], "k_");
}

fn eatToken(p: *Self, tag: Token.Tag) ?TokenIndex {
    return if (p.peekToken(.tag, 0) == tag) p.advanceToken() else null;
}

fn eatKeyword(p: *Self) ?TokenIndex {
    return if (isKeyword(p.peekToken(.tag, 0))) p.advanceToken() else null;
}

fn expectToken(p: *Self, tag: Token.Tag) Error!TokenIndex {
    const token = p.advanceToken();
    if (p.tokens.items(.tag)[token] == tag) return token;

    try p.addErrorAdvanced(.expected_token, token, 0, @intFromEnum(tag));
    return Error.Parsing;
}

fn getOrPutIdentAdvanced(
    p: *Self,
    comptime is_literal: bool,
    token: TokenIndex,
) Allocator.Error!Node.IdentIndex {
    var src = p.tokenSource(token);
    if (is_literal) src = src[1 .. src.len - 1];
    return try p.builder.getOrPutIdent(p.allocator, src);
}

fn getOrPutIdent(p: *Self, token: TokenIndex) Allocator.Error!Node.IdentIndex {
    return try p.getOrPutIdentAdvanced(false, token);
}

fn expectIdent(p: *Self) Error!Node.IdentIndex {
    const token = try p.expectToken(.ident);
    return try p.getOrPutIdent(token);
}

fn tokenSource(p: Self, i: TokenIndex) []const u8 {
    const loc = p.tokens.items(.loc)[i];
    return p.source[loc.start..loc.end];
}

fn expectAttribEnd(p: *Self) Error!void {
    _ = p.eatToken(.@",");
    _ = try p.expectToken(.@")");
}

fn expectEnum(p: *Self, comptime T: type, err_tag: ParsingError.Tag) !Node.Index {
    const tok = try p.expectToken(.ident);
    const enum_val = std.meta.stringToEnum(T, p.tokenSource(tok)) orelse {
        try p.addError(err_tag, tok);
        return Error.Parsing;
    };
    return @intFromEnum(enum_val);
}

/// '(' severity_control_name ',' diagnostic_rule_name attrib_end
/// diagnostic_rule_name:
///   | diagnostic_name_token
///   | diagnostic_name_token '.' diagnostic_name_token
fn expectDiagnosticControl(p: *Self) Error!Node.Index {
    _ = try p.expectToken(.@"(");
    const sev = try p.expectEnum(Node.Severity, .invalid_severity);
    _ = try p.expectToken(.@",");

    var control = Node.DiagnosticControl{
        .severity = sev,
        .name = try p.expectIdent(),
    };
    if (p.eatToken(.@".")) |_| control.field = try p.expectIdent();
    try p.expectAttribEnd();

    return try p.addExtra(control);
}

// 'diagnostic' diagnostic_control ';'
fn diagnosticDirective(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_diagnostic) orelse return null;
    const control = try p.expectDiagnosticControl();
    _ = try p.expectToken(.@";");
    return try p.addNode(tok, .diagnostic_directive, control, 0);
}

/// | enable_extension_list :
///   enable_extension_name ( ',' enable_extension_name ) * ',' ?
/// | software_extension_list :
///   software_extension_name ( `','` software_extension_name ) * `','` ?
fn identTokenList(p: *Self) Error!Node.Index {
    const tok = p.tok_i;
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const ident_tok = p.eatKeyword() orelse try p.expectToken(.ident);
        const ident = try p.getOrPutIdent(ident_tok);
        try p.scratch.append(p.allocator, ident);
        if (p.eatToken(.@",") == null) break;
    }
    _ = p.eatToken(.@",");
    return try p.listToSpan(tok, p.scratch.items[scratch_top..]);
}

/// 'enable' enable_extension_list ';'
fn enableDirective(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_enable) orelse return null;
    const extensions = try p.identTokenList();
    _ = try p.expectToken(.@";");
    return try p.addNode(tok, .enable_directive, extensions, 0);
}

/// 'requires' software_extension_list ';'
fn requiresDirective(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_requires) orelse return null;
    const extensions = try p.identTokenList();
    _ = try p.expectToken(.@";");
    return try p.addNode(tok, .requires_directive, extensions, 0);
}

/// diagnostic_directive | enable_directive | requires_directive | import_directive
fn globalDirective(p: *Self) Error!?Node.Index {
    return try p.diagnosticDirective() orelse
        try p.enableDirective() orelse
        try p.requiresDirective() orelse
        try p.importDirective();
}

fn findNextGlobalDirective(p: *Self) void {
    while (true) {
        switch (p.peekToken(.tag, 0)) {
            .k_enable, .k_requires, .k_diagnostic, .k_import, .eof => return,
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
        try p.addError(.expected_attribute_decl, null);
        return Error.Parsing;
    }
    if (try p.constDecl() orelse
        try p.typeAliasDecl() orelse
        try p.constAssertStatement()) |node|
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
    const tok = p.eatToken(.@"@") orelse return null;
    const ident_token = p.eatToken(.k_diagnostic) orelse try p.expectToken(.ident);
    const str = p.tokenSource(ident_token);
    const tag = std.meta.stringToEnum(Node.Attribute, str) orelse {
        try p.addError(.invalid_attribute, ident_token);
        return Error.Parsing;
    };

    const attr: Node.Index = switch (tag) {
        .compute, .@"const", .fragment, .invariant, .must_use, .vertex => 0,
        .@"align", .binding, .group, .id, .location, .size => brk: {
            _ = try p.expectToken(.@"(");
            const e = try p.expectExpression();
            try p.expectAttribEnd();
            break :brk e;
        },
        .builtin => brk: {
            _ = try p.expectToken(.@"(");
            const e = try p.expectEnum(Node.Builtin, .invalid_builtin);
            try p.expectAttribEnd();
            break :brk e;
        },
        .diagnostic => try p.expectDiagnosticControl(),
        .interpolate => brk: {
            _ = try p.expectToken(.@"(");
            var interpolation = Node.Interpolation{
                .type = try p.expectEnum(Node.InterpolationType, .invalid_interpolation_type),
            };
            if (p.eatToken(.@",") != null and p.peekToken(.tag, 0) != .@")") {
                interpolation.sampling_expr = try p.expectEnum(Node.InterpolationSampling, .invalid_interpolation_sampling);
            }
            try p.expectAttribEnd();
            break :brk try p.addExtra(interpolation);
        },
        .workgroup_size => brk: {
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
            break :brk try p.addExtra(workgroup_size);
        },
    };

    return try p.addNode(tok, .attribute, @intFromEnum(tag), attr);
}

/// attribute*
fn attributeList(p: *Self) Error!Node.Index {
    const tok = p.tok_i;
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        if (try p.attribute()) |a| try p.scratch.append(p.allocator, a) else break;
    }
    return try p.listToSpan(tok, p.scratch.items[scratch_top..]);
}

const Ident = struct {
    name: Node.IdentIndex,
    type: Node.Index = 0,
    initializer: Node.Index = 0,
};

/// ident ( ':' type_specifier )?
fn expectOptionallyTypedIdent(p: *Self) Error!Ident {
    var res = Ident{ .name = try p.expectIdent() };
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
    return try p.variableDecl(attrs);
}

/// attribute * 'override' optionally_typed_ident ( '=' expression ) ?
fn globalOverrideDecl(p: *Self, attrs: Node.Index) Error!?Node.Index {
    const tok = p.eatToken(.k_override) orelse return null;
    const ident = try p.expectOptionallyTypedIdentWithInitializer();
    const extra = try p.addExtra(Node.Override{
        .attrs = attrs,
        .name = ident.name,
        .type = ident.type,
    });
    return try p.addNode(tok, .override, extra, ident.initializer);
}

/// 'alias' ident '=' type_specifier
fn typeAliasDecl(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_alias) orelse return null;
    const new_name = try p.expectIdent();
    _ = try p.expectToken(.@"=");
    const old_type = try p.expectTypeSpecifier();
    return try p.addNode(tok, .type_alias, new_name, old_type);
}

/// attribute* member_ident ':' type_specifier
fn structMember(p: *Self, attrs: Node.Index) Error!?Node.Index {
    const tok = p.eatToken(.ident) orelse return null;
    _ = try p.expectToken(.@":");
    const member_type = try p.expectTypeSpecifier();
    const extra = try p.addExtra(Node.TypedIdent{
        .name = try p.getOrPutIdent(tok),
        .type = member_type,
    });
    return try p.addNode(tok, .struct_member, attrs, extra);
}

/// 'struct' ident '{' struct_member ( ',' struct_member ) * ',' ? '}'
fn structDecl(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_struct) orelse return null;
    const name_token = try p.expectToken(.ident);

    const span_tok = try p.expectToken(.@"{");
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

    const name = try p.getOrPutIdent(name_token);
    const member_span = try p.listToSpan(span_tok, members);
    return try p.addNode(tok, .@"struct", name, member_span);
}

/// 'const_assert' expression
fn constAssertStatement(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_const_assert) orelse return null;
    const expr = try p.expectExpression();
    return try p.addNode(tok, .const_assert, expr, 0);
}

/// ident '(' param_list? ')' ( '->' attribute* template_elaborated_ident )?
fn fnHeader(p: *Self, attrs: Node.Index) Error!Node.Index {
    const name = try p.expectIdent();
    _ = try p.expectToken(.@"(");
    const params = try p.fnParameterList();
    _ = try p.expectToken(.@")");

    var proto = Node.FnHeader{
        .attrs = attrs,
        .name = name,
        .params = params,
    };
    if (p.eatToken(.@"->")) |_| {
        proto.return_attrs = try p.attributeList();
        proto.return_type = try p.expectTemplateElaboratedIdent();
    }
    return try p.addExtra(proto);
}

/// attribute* 'fn' function_header compound_statement
fn fnDecl(p: *Self, attrs: Node.Index) Error!?Node.Index {
    const tok = p.eatToken(.k_fn) orelse return null;
    const fn_header = try p.fnHeader(attrs);
    const body = try p.expectCompoundStatement(try p.attributeList());
    return try p.addNode(tok, .@"fn", fn_header, body);
}

// ident ('as' ident)?
fn importAlias(p: *Self) Error!?Node.Index {
    const tok = try p.expectToken(.ident);
    const old = try p.getOrPutIdent(tok);
    const new = if (p.eatToken(.k_as)) |_| try p.expectIdent() else 0;
    return try p.addNode(tok, .import_alias, old, new);
}

// | '*'
// | '{' import_alias (',' import_alias)* ','? '}'
fn importAliasList(p: *Self) Error!Node.Index {
    if (p.eatToken(.@"*")) |_| return 0;
    const tok = try p.expectToken(.@"{");
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        if (try p.importAlias()) |a| try p.scratch.append(p.allocator, a);
        if (p.eatToken(.@",") == null) break;
    }
    _ = try p.expectToken(.@"}");
    const imports = p.scratch.items[scratch_top..];
    return try p.listToSpan(tok, imports);
}

/// 'import' import_list 'from' string_literal ';'?
fn importDirective(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_import) orelse return null;
    const importAliases = try p.importAliasList();
    _ = try p.expectToken(.k_from);
    const mod_token = try p.expectToken(.string);
    _ = p.eatToken(.@";");
    const module = try p.getOrPutIdentAdvanced(true, mod_token);
    return try p.addNode(tok, .import, importAliases, module);
}

/// attribute* ident ':' type_specifier
fn fnParameter(p: *Self) Error!?Node.Index {
    const attrs = try p.attributeList();
    const tok = p.eatToken(.ident) orelse {
        if (attrs != 0) {
            try p.addError(.expected_function_parameter, null);
            return Error.Parsing;
        }
        return null;
    };
    _ = try p.expectToken(.@":");
    const param_type = try p.expectTypeSpecifier();
    const extra = try p.addExtra(Node.FnParam{ .name = try p.getOrPutIdent(tok), .type = param_type });
    return try p.addNode(tok, .fn_param, attrs, extra);
}

/// param ( ',' param )* ','?
fn fnParameterList(p: *Self) Error!Node.Index {
    const tok = p.tok_i;
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        if (try p.fnParameter()) |pa| try p.scratch.append(p.allocator, pa);
        if (p.eatToken(.@",") == null) break;
    }
    return try p.listToSpan(tok, p.scratch.items[scratch_top..]);
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
        try p.addError(.expected_attribute_statement, null);
        return Error.Parsing;
    }

    if (try p.ifStatement() orelse
        try p.switchStatement() orelse
        try p.whileStatement()) |node| return node;
    if (try p.returnStatement() orelse
        try p.callStatement() orelse
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

/// | variable_decl
/// | variable_decl '=' expression
/// | 'let' optionally_typed_ident '=' expression
/// | 'const' optionally_typed_ident '=' expression
fn variableOrValueStatement(p: *Self) Error!?Node.Index {
    return try p.variableDecl(0) orelse try p.letDecl() orelse try p.constDecl();
}

/// 'return' expression ?
fn returnStatement(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_return) orelse return null;
    const expr = try p.expression() orelse 0;
    return try p.addNode(tok, .@"return", expr, 0);
}

/// attribute* if_clause else_if_clause* else_clause?
/// if_clause :
///  'if' expression compound_statement
/// else_if_clause :
///  'else' 'if' expression compound_statement
/// else_clause :
///  'else' compound_statement
fn ifStatement(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_if) orelse return null;

    const condition = try p.expectExpression();
    const body = try p.expectCompoundStatement(try p.attributeList());
    const if_node = try p.addNode(tok, .@"if", condition, body);

    if (p.eatToken(.k_else)) |t| {
        if (p.peekToken(.tag, 0) == .k_if) {
            // Instead of building a list of "else if" clauses, use recursion.
            const else_if = try p.ifStatement() orelse unreachable;
            return try p.addNode(t, .else_if, if_node, else_if);
        } else {
            const else_body = try p.expectCompoundStatement(try p.attributeList());
            return try p.addNode(t, .@"else", if_node, else_body);
        }
    }

    return if_node;
}

/// attribute* '{' statement* last_statement? '}'
fn compoundStatementExtra(p: *Self, attrs: Node.Index, last_statement: anytype) Error!?Node.Index {
    const tok = p.eatToken(.@"{") orelse return null;
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        if (try p.statement()) |s| try p.scratch.append(p.allocator, s) else break;
    }
    if (try last_statement(p)) |s| try p.scratch.append(p.allocator, s);
    _ = try p.expectToken(.@"}");
    const statements = try p.listToSpan(tok, p.scratch.items[scratch_top..]);
    return try p.addNode(tok, .compound, attrs, statements);
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
        const tok = p.advanceToken();
        return try p.addNode(tok, .@"break", 0, 0);
    }
    return null;
}

/// 'break' 'if' expression ';'
fn breakIfStatement(p: *Self) Error!?Node.Index {
    if (p.peekToken(.tag, 0) == .k_break and p.peekToken(.tag, 1) == .k_if) {
        const tok = p.advanceToken();
        _ = p.advanceToken();
        const expr = try p.expectExpression();
        _ = try p.expectToken(.@";");
        return try p.addNode(tok, .break_if, expr, 0);
    }
    return null;
}

/// 'continue'
fn continueStatement(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_continue) orelse return null;
    return try p.addNode(tok, .@"continue", 0, 0);
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
    const tok = p.eatToken(.k_continuing) orelse return null;
    const body = try p.expectContinuingCompoundStatement();
    return try p.addNode(tok, .continuing, body, 0);
}

fn discardStatement(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_discard) orelse return null;
    return try p.addNode(tok, .discard, 0, 0);
}

/// | variable_or_value_statement
/// | variable_updating_statement
/// | func_call_statement
fn forInit(p: *Self) Error!?Node.Index {
    return try p.variableOrValueStatement() orelse try p.callStatement() orelse try p.variableUpdatingStatement();
}

/// | variable_updating_statement
/// | func_call_statement
fn forUpdate(p: *Self) Error!?Node.Index {
    return try p.variableUpdatingStatement() orelse try p.callStatement();
}

/// for_init? ';' expression? ';' for_update?
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
    const tok = p.eatToken(.k_for) orelse return null;
    _ = try p.expectToken(.@"(");
    const header = try p.expectForHeader(attrs);
    _ = try p.expectToken(.@")");
    const body = try p.expectCompoundStatement(try p.attributeList());

    return try p.addNode(tok, .@"for", header, body);
}

// attribute* 'loop' attribute* '{' statement * continuing_statement ? '}'
fn loopStatement(p: *Self, attrs: Node.Index) Error!?Node.Index {
    const tok = p.eatToken(.k_loop) orelse return null;
    const body = try p.compoundStatementExtra(try p.attributeList(), continuingStatement) orelse {
        try p.addError(.expected_compound_statement, null);
        return Error.Parsing;
    };
    return try p.addNode(tok, .loop, attrs, body);
}

/// 'default' | expression
fn caseSelector(p: *Self) Error!?Node.Index {
    if (p.eatToken(.k_default)) |t| {
        return try p.addNode(t, .case_selector, 0, 0);
    } else {
        const tok = p.tok_i;
        if (try p.expression()) |e| {
            return try p.addNode(tok, .case_selector, e, 0);
        }
    }
    return null;
}

/// 'case' case_selectors ':' ? compound_statement
/// case_selectors :
///   case_selector ( ',' case_selector ) * ',' ?
fn caseClause(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_case) orelse return null;
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
    const selectors = try p.listToSpan(tok + 1, p.scratch.items[scratch_top..]);
    const body = try p.expectCompoundStatement(try p.attributeList());
    return try p.addNode(tok, .case_clause, selectors, body);
}

/// 'default' ':'? compound_statement
fn defaultAloneClause(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_default) orelse return null;
    _ = p.eatToken(.@":");
    const body = try p.expectCompoundStatement(try p.attributeList());
    return try p.addNode(tok, .case_clause, 0, body);
}

/// case_clause | default_alone_clause
fn switchClause(p: *Self) Error!?Node.Index {
    return try p.caseClause() orelse try p.defaultAloneClause();
}

/// attribute* '{' switch_clause + '}'
fn switchBody(p: *Self) Error!Node.Index {
    const attrs = try p.attributeList();
    const tok = try p.expectToken(.@"{");
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        if (try p.switchClause()) |c| try p.scratch.append(p.allocator, c) else break;
    }
    _ = try p.expectToken(.@"}");

    const clauses = try p.listToSpan(tok, p.scratch.items[scratch_top..]);
    return try p.addNode(tok, .switch_body, attrs, clauses);
}

/// attribute* 'switch' expression switch_body
fn switchStatement(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_switch) orelse return null;
    const expr = try p.expectExpression();
    const switch_body = try p.switchBody();
    return try p.addNode(tok, .@"switch", expr, switch_body);
}

const VarTemplate = struct { address_space: Node.Index = 0, access_mode: Node.Index = 0 };
fn varTemplate(p: *Self) Error!VarTemplate {
    var res = VarTemplate{};
    if (p.eatToken(.template_args_start)) |_| {
        res.address_space = try p.expectEnum(Node.AddressSpace, .invalid_address_space);
        if (p.eatToken(.@",") != null and p.peekToken(.tag, 0) != .template_args_end) {
            res.access_mode = try p.expectEnum(Node.AccessMode, .invalid_access_mode);
        }
        _ = p.eatToken(.@",");
        _ = try p.expectToken(.template_args_end);
    }

    return res;
}

// | 'var' template_list? optionally_typed_ident
// | 'var' template_list? optionally_typed_ident '=' expression
fn variableDecl(p: *Self, attrs: Node.Index) Error!?Node.Index {
    const tok = p.eatToken(.k_var) orelse return null;
    const template = try p.varTemplate();
    const ident = try p.expectOptionallyTypedIdentWithInitializer();
    const extra = try p.addExtra(Node.Var{
        .attrs = attrs,
        .name = ident.name,
        .address_space = template.address_space,
        .access_mode = template.access_mode,
        .type = ident.type,
    });
    return try p.addNode(tok, .@"var", extra, ident.initializer);
}

/// 'let' optionally_typed_ident '=' expression
fn letDecl(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_let) orelse return null;
    const ident = try p.expectOptionallyTypedIdentWithInitializer();
    const extra = try p.addExtra(Node.TypedIdent{
        .name = ident.name,
        .type = ident.type,
    });
    return try p.addNode(tok, .let, extra, ident.initializer);
}

/// 'const' optionally_typed_ident '=' expression
fn constDecl(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_const) orelse return null;
    const ident = try p.expectOptionallyTypedIdentWithInitializer();
    const extra = try p.addExtra(Node.TypedIdent{
        .name = ident.name,
        .type = ident.type,
    });
    return try p.addNode(tok, .@"const", extra, ident.initializer);
}

/// | '_' '=' expression
/// | lhs_expression ( '=' | compound_assignment_operator ) expression
/// | lhs_expression '++'
/// | lhs_expression '--'
fn variableUpdatingStatement(p: *Self) Error!?Node.Index {
    if (p.eatToken(._)) |_| {
        const tok = try p.expectToken(.@"=");
        const expr = try p.expectExpression();
        return try p.addNode(tok, .phony_assign, 0, expr);
    } else if (try p.lhsExpression()) |lhs| {
        const tok = p.advanceToken();
        switch (p.tokens.items(.tag)[tok]) {
            .@"++" => return try p.addNode(tok, .increment, lhs, 0),
            .@"--" => return try p.addNode(tok, .decrement, lhs, 0),
            inline .@"=",
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
            => |t| {
                const rhs = try p.expectExpression();
                const tag: Node.Tag = switch (t) {
                    .@"=" => .@"=",
                    .@"+=" => .@"+=",
                    .@"-=" => .@"-=",
                    .@"*=" => .@"*=",
                    .@"/=" => .@"/=",
                    .@"%=" => .@"%=",
                    .@"&=" => .@"&=",
                    .@"|=" => .@"|=",
                    .@"^=" => .@"^=",
                    .@"<<=" => .@"<<=",
                    .@">>=" => .@">>=",
                    else => unreachable,
                };
                return try p.addNode(tok, tag, lhs, rhs);
            },
            else => {
                try p.addError(.invalid_assignment_op, tok);
                return Error.Parsing;
            },
        }
    }

    return null;
}

fn whileStatement(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.k_while) orelse return null;
    const cond = try p.expectExpression();
    const body = try p.expectCompoundStatement(try p.attributeList());
    return try p.addNode(tok, .@"while", cond, body);
}

/// template_elaborated_ident
fn typeSpecifier(p: *Self) Error!?Node.Index {
    if (try p.templateElaboratedIdent()) |n| {
        p.builder.nodes.items(.tag)[n] = .ident;
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
    const tok = p.eatToken(.@"(") orelse return null;
    const expr = try p.expectExpression();
    _ = try p.expectToken(.@")");
    return try p.addNode(tok, .paren, expr, 0);
}

/// expression ( ',' expression ) * ',' ?
fn expressionCommaList(p: *Self) Error!Node.Index {
    const tok = p.tok_i;
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const expr = try p.expression() orelse break;
        try p.scratch.append(p.allocator, expr);
        if (p.eatToken(.@",") == null) break;
    }
    return try p.listToSpan(tok, p.scratch.items[scratch_top..]);
}

/// '(' expression_comma_list ? ')'
fn expectArgumentExpressionList(p: *Self) Error!Node.Index {
    _ = try p.expectToken(.@"(");
    const res = try p.expressionCommaList();
    try p.expectAttribEnd();
    return res;
}

/// template_elaborated_ident argument_expression_list
fn callPhrase(p: *Self) Error!?Node.Index {
    const tok = p.tok_i;
    const ident = try p.templateElaboratedIdent() orelse return null;
    // Unfortunately need lookbehind since sometimes other `templateElaboratedIdent` rules follow.
    if (p.peekToken(.tag, 0) != .@"(") {
        p.tok_i = tok;
        return null;
    }
    const arguments = try p.expectArgumentExpressionList();
    return try p.addNode(tok, .call, ident, arguments);
}

fn callExpr(p: *Self) Error!?Node.Index {
    return try p.callPhrase();
}

fn callStatement(p: *Self) Error!?Node.Index {
    return try p.callPhrase();
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
        const name = try p.getOrPutIdent(tok);
        return try p.addNode(tok, .ident, name, 0);
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
    if (p.eatToken(.@"*")) |t| {
        const expr = try p.expectLhsExpression();
        return try p.addNode(t, .deref, expr, 0);
    }
    if (p.eatToken(.@"&")) |t| {
        const expr = try p.expectLhsExpression();
        return try p.addNode(t, .ref, expr, 0);
    }
    return null;
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
        try p.callExpr() orelse
        try p.templateElaboratedIdent();
}

/// '<' expression ( ',' expression ) * ',' ? '>'
fn templateList(p: *Self) Error!Node.Index {
    const tok = p.eatToken(.template_args_start) orelse return 0;
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);
    const expr = try p.expectExpression();
    try p.scratch.append(p.allocator, expr);
    while (p.eatToken(.@",") != null) {
        if (try p.expression()) |e| try p.scratch.append(p.allocator, e) else break;
    }
    _ = try p.expectToken(.template_args_end);
    return try p.listToSpan(tok, p.scratch.items[scratch_top..]);
}

/// ident template_list?
fn templateElaboratedIdent(p: *Self) Error!?Node.Index {
    const tok = p.eatToken(.ident) orelse return null;
    const name = try p.getOrPutIdent(tok);
    const template_list = try p.templateList();
    return try p.addNode(tok, .ident, name, template_list);
}

fn expectTemplateElaboratedIdent(p: *Self) Error!Node.Index {
    return try p.templateElaboratedIdent() orelse {
        try p.addError(.expected_template_elaborated_ident, null);
        return Error.Parsing;
    };
}

/// | int_literal
/// | float_literal
/// | bool_literal
fn literal(p: *Self) Error!?Node.Index {
    if (p.eatToken(.k_true)) |t| return try p.addNode(t, .true, 0, 0);
    if (p.eatToken(.k_false)) |t| return try p.addNode(t, .false, 0, 0);
    if (p.eatToken(.number)) |t| {
        const num = parseNumber(p.tokenSource(t)) catch |err| switch (err) {
            error.InvalidCharacter => {
                try p.addError(.invalid_character, null);
                return Error.Parsing;
            },
        };
        switch (num) {
            .abstract_int => |i| {
                const int_bits: u64 = @bitCast(i);
                return try p.addNode(t, .abstract_int, @truncate(int_bits), @truncate(int_bits >> 32));
            },
            .i32 => |i| {
                return try p.addNode(t, .i32, @bitCast(i), 0);
            },
            .u32 => |i| {
                return try p.addNode(t, .u32, i, 0);
            },
            .abstract_float => |f| {
                const int_bits: u64 = @bitCast(f);
                return try p.addNode(t, .abstract_float, @truncate(int_bits), @truncate(int_bits >> 32));
            },
            .f32 => |f| {
                return try p.addNode(t, .f32, @bitCast(f), 0);
            },
            .f16 => |f| {
                const int_bits: u16 = @bitCast(f);
                return try p.addNode(t, .f16, int_bits, 0);
            },
            inline .failure => |e| {
                const tag = std.meta.stringToEnum(ParsingError.Tag, @tagName(e)) orelse {
                    std.debug.print("tag {s} not in ParsingError.Tag\n", .{@tagName(e)});
                    unreachable;
                };
                var offset: Loc.Index = 0;
                var extra: Loc.Index = 0;
                switch (e) {
                    .invalid_digit => |d| {
                        offset = d.i;
                        extra = @intFromEnum(d.base);
                    },
                    .duplicate_exponent, .trailing_special, .invalid_character, .invalid_exponent_sign, .trailing_chars => |o| offset = o,
                    else => {},
                }
                try p.addErrorAdvanced(tag, t, offset, extra);
            },
        }
    }
    return null;
}

/// | singular_expression
/// | `'!'` unary_expression
/// | `'~'` unary_expression
/// | `'-'` unary_expression
/// | `'*'` unary_expression
/// | `'&'` unary_expression
fn unaryExpr(p: *Self) Error!?Node.Index {
    return switch (p.peekToken(.tag, 0)) {
        inline .@"!", .@"~", .@"-", .@"*", .@"&" => |t| brk: {
            const tok = p.advanceToken();
            const expr = try p.expectUnaryExpr();
            const tag: Node.Tag = switch (t) {
                .@"!" => .logical_not,
                .@"~" => .bitwise_complement,
                .@"-" => .negative,
                .@"*" => .deref,
                .@"&" => .ref,
                else => unreachable,
            };
            break :brk try p.addNode(tok, tag, expr, 0);
        },
        else => p.singularExpr(),
    };
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
        inline .@"<", .@">", .@"<=", .@">=", .@"==", .@"!=" => |t| {
            const tag: Node.Tag = switch (t) {
                .@"<" => .lt,
                .@">" => .gt,
                .@"<=" => .lte,
                .@">=" => .gte,
                .@"==" => .eq,
                .@"!=" => .neq,
                else => unreachable,
            };
            const tok = p.advanceToken();
            const unary = try p.expectUnaryExpr();
            const rhs = try p.shiftExpr(unary);
            return try p.addNode(tok, tag, lhs, rhs);
        },
        else => return lhs,
    }
}

/// | relational_expression `'||'` relational_expression
/// | relational_expression `'&&'` relational_expression
fn shortCircuitExpr(p: *Self, lhs_relational: Node.Index) Error!Node.Index {
    const op_token = p.tok_i;
    switch (p.tokens.items(.tag)[op_token]) {
        inline .@"&&", .@"||" => |t| {
            const tag: Node.Tag = switch (t) {
                .@"&&" => .logical_and,
                .@"||" => .logical_or,
                else => unreachable,
            };
            var lhs = lhs_relational;
            while (p.peekToken(.tag, 0) == p.tokens.items(.tag)[op_token]) {
                const tok = p.advanceToken();
                const rhs_unary = try p.expectUnaryExpr();
                const rhs = try p.relationalExpr(rhs_unary);
                lhs = try p.addNode(tok, tag, lhs, rhs);
            }

            return lhs;
        },
        else => return lhs_relational,
    }
}

/// | binary_and_expression `'&'` unary_expression
/// | binary_or_expression `'|'` unary_expression
/// | binary_xor_expression `'^'` unary_expression
fn bitwiseExpr(p: *Self, lhs: Node.Index) Error!?Node.Index {
    switch (p.peekToken(.tag, 0)) {
        inline .@"&", .@"|", .@"^" => |t| {
            const tag: Node.Tag = switch (t) {
                .@"&" => .bitwise_and,
                .@"|" => .bitwise_or,
                .@"^" => .bitwise_xor,
                else => unreachable,
            };
            const tok = p.advanceToken();
            var lhs_result = lhs;
            while (true) {
                const rhs = try p.expectUnaryExpr();
                lhs_result = try p.addNode(tok, tag, lhs_result, rhs);

                if (p.peekToken(.tag, 0) != p.tokens.items(.tag)[tok]) return lhs_result;
            }
        },
        else => return null,
    }
}

/// | additive_expression
/// | unary_expression _shift_left unary_expression
/// | unary_expression _shift_right unary_expression
fn shiftExpr(p: *Self, lhs: Node.Index) Error!Node.Index {
    switch (p.peekToken(.tag, 0)) {
        inline .@"<<", .@">>" => |t| {
            const tag: Node.Tag = switch (t) {
                .@"<<" => .lshift,
                .@">>" => .rshift,
                else => unreachable,
            };
            const tok = p.advanceToken();
            const rhs = try p.expectUnaryExpr();
            return try p.addNode(tok, tag, lhs, rhs);
        },
        else => return try p.mathExpr(lhs),
    }
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
            inline .@"+", .@"-" => |t| {
                const tag: Node.Tag = switch (t) {
                    .@"+" => .add,
                    .@"-" => .sub,
                    else => unreachable,
                };
                const tok = p.advanceToken();
                const unary = try p.expectUnaryExpr();
                const rhs = try p.multiplicativeExpr(unary);
                lhs = try p.addNode(tok, tag, lhs, rhs);
            },
            else => return lhs,
        }
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
            inline .@"*", .@"/", .@"%" => |t| {
                const tok = p.advanceToken();
                const tag: Node.Tag = switch (t) {
                    .@"*" => .mul,
                    .@"/" => .div,
                    .@"%" => .mod,
                    else => unreachable,
                };
                const rhs = try p.expectUnaryExpr();
                lhs = try p.addNode(tok, tag, lhs, rhs);
            },
            else => return lhs,
        }
    }
}

/// | '[' expression ']' component_or_swizzle_specifier ?
/// | '.' member_ident component_or_swizzle_specifier ?
/// | '.' swizzle_name component_or_swizzle_specifier ?
fn componentOrSwizzle(p: *Self, prefix: Node.Index) Error!Node.Index {
    var prefix_result = prefix;
    while (true) {
        if (p.eatToken(.@".")) |t| {
            const field = try p.expectIdent();
            prefix_result = try p.addNode(t, .field_access, prefix_result, field);
        } else if (p.eatToken(.@"[")) |t| {
            const index_expr = try p.expectExpression();
            _ = try p.expectToken(.@"]");
            prefix_result = try p.addNode(t, .index_access, prefix_result, index_expr);
        } else return prefix_result;
    }
}

test "parser error" {
    const source =
        \\const a: u32 = 0u;
        \\not valid wgsl;
        \\const b: u32 = 0u;
    ;
    const allocator = std.testing.allocator;
    var tree = try parse(allocator, null, source);
    defer tree.deinit(allocator);

    const roots = tree.spanToList(0);
    try std.testing.expectEqual(@as(usize, 2), roots.len);
    try std.testing.expectEqual(@as(usize, 1), tree.errors.len);
    const expected_err = File.Error{
        .path = null,
        .source = source,
        .loc = .{
            .line_num = 2,
            .line_start = 19,
            .tok_start = 19,
            .tok_end = 22,
        },
        .data = .{ .wgsl = .{ .tag = ParsingError.Tag.expected_global_decl } },
    };
    try std.testing.expectEqual(expected_err, tree.errors[0]);
}
