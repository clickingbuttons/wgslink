const std = @import("std");
const Parser = @import("Parser.zig");
const Token = @import("Token.zig");
const Node = @import("Node.zig");
const Tokenizer = @import("Tokenizer.zig").Tokenizer;

const Allocator = std.mem.Allocator;
const Self = @This();
pub const ByteOffset = Token.Index;
pub const TokenList = std.MultiArrayList(Token);
pub const NodeList = std.MultiArrayList(Node);
pub const Error = struct {
    tag: Tag,
    token: Token.Index,
    expected_tag: ?Token.Tag = null,

    pub const Tag = enum {
        invalid_attribute,
        invalid_attributes,
        invalid_element_count,
        invalid_initializer,
        invalid_assignment_op,
        invalid_severity,
        expected_token,
        expected_unary_expr,
        expected_expr,
        expected_lhs_expr,
        expected_struct_member,
        expected_function_parameter,
        expected_global_decl,
        expected_global_directive,
        expected_compound_statement,
        expected_continuing_compound_statement,
        expected_statement,
        expected_type_specifier,
        expected_continuing_statement,
        expected_case_selector,
        type_needs_ext,
        empty_struct,
        deep_template,
        unresolved_module,
    };
};

source: [:0]const u8,
tokens: TokenList.Slice,
/// Node 0 is a span of directives followed by declarations. Since there can be no
/// references to this root node, 0 is available to indicate null.
nodes: NodeList.Slice,
extra: []Node.Index,
/// Point to source.
errors: []const Error,

pub fn init(allocator: std.mem.Allocator, source: [:0]const u8) Allocator.Error!Self {
    var tokens = TokenList{};
    defer tokens.deinit(allocator);
    try tokens.ensureTotalCapacity(allocator, source.len / 8);

    var tokenizer = Tokenizer.init(source);
    while (true) {
        const token = tokenizer.next();
        try tokens.append(allocator, token);
        if (token.tag == .eof) break;
    }

    var parser = try Parser.init(allocator, source, tokens);
    defer parser.deinit(allocator);

    try parser.parseTranslationUnit();

    return Self{
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra = try parser.extra.toOwnedSlice(allocator),
        .errors = try parser.errors.toOwnedSlice(allocator),
    };
}

pub fn deinit(self: *Self, allocator: Allocator) void {
    self.tokens.deinit(allocator);
    self.nodes.deinit(allocator);
    allocator.free(self.extra);
    allocator.free(self.errors);
    self.* = undefined;
}

pub fn rootDecls(self: Self) []const Node.Index {
    // Root is always index 0.
    const node = self.nodes.items(.data)[0];
    return self.extra_data[node.lhs..node.rhs];
}

pub fn extraData(self: Self, comptime T: type, index: Node.Index) T {
    const fields: []const std.builtin.Type.StructField = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        @field(result, field.name) = self.extra[index + i];
    }
    return result;
}

fn tokenTag(self: Self, i: Token.Index) Token.Tag {
    return self.tokens.items(.tag)[i];
}

fn tokenLoc(self: Self, i: Token.Index) Token.Loc {
    return self.tokens.items(.loc)[i];
}

pub fn tokenSource(self: Self, i: Token.Index) []const u8 {
    const loc = self.tokenLoc(i);
    return self.source[loc.start..loc.end];
}

pub fn nodeTag(self: Self, i: Node.Index) Node.Tag {
    return self.nodes.items(.tag)[i];
}

pub fn nodeToken(self: Self, i: Node.Index) Token.Index {
    return self.nodes.items(.main_token)[i];
}

pub fn nodeLHS(self: Self, i: Node.Index) Node.Index {
    return self.nodes.items(.lhs)[i];
}

pub fn nodeRHS(self: Self, i: Node.Index) Node.Index {
    return self.nodes.items(.rhs)[i];
}

pub fn nodeSource(self: Self, i: Node.Index) []const u8 {
    var loc = self.tokenLoc(self.nodeToken(i));
    switch (self.nodeTag(i)) {
        .deref, .addr_of => {
            const lhs = self.nodeLHS(i);
            const lhs_token = self.nodeToken(lhs);
            const lhs_loc = self.tokenLoc(lhs_token);
            loc.end = lhs_loc.end;
        },
        .field_access => {
            const component_loc = self.tokenLoc(self.nodeToken(i) + 1);
            loc.end = component_loc.end;
        },
        else => {},
    }
    return self.source[loc.start..loc.end];
}

pub fn aliasName(self: Self, i: Node.Index) []const u8 {
    std.debug.assert(self.nodeTag(i) == .alias);
    const lhs = self.nodeLHS(i);
    if (lhs != 0) return self.tokenSource(lhs);

    return self.nodeSource(i);
}

fn declNameLoc(self: Self, i: Node.Index) ?Token.Loc {
    const token: Token.Index = switch (self.nodeTag(i)) {
        .global_var => self.extraData(Node.GlobalVar, self.nodeLHS(i)).name,
        .@"var" => self.extraData(Node.Var, self.nodeLHS(i)).name,
        .@"struct",
        .@"fn",
        .@"const",
        .let,
        .override,
        .type_alias,
        => self.nodeToken(i) + 1,
        .struct_member, .fn_param => self.nodeToken(i),
        else => return null,
    };
    return self.tokens.items(.loc)[token];
}

pub fn declNameSource(self: Self, i: Node.Index) []const u8 {
    const loc = self.declNameLoc(i).?;
    return self.source[loc.start..loc.end];
}

pub fn spanToList(self: Self, i: Node.Index) []const Node.Index {
    std.debug.assert(self.nodeTag(i) == .span);
    return @ptrCast(self.extra[self.nodeLHS(i)..self.nodeRHS(i)]);
}

pub fn moduleName(self: Self, i: Node.Index) []const u8 {
    std.debug.assert(self.nodeTag(i) == .import);
    const token = self.tokenSource(self.nodeRHS(i));
    return token[1 .. token.len - 1];
}

fn renderList(writer: anytype, Enum: anytype) !void {
    try writer.writeAll(". expected one of ");
    const fields = @typeInfo(Enum).Enum.fields;
    inline for (fields, 0..) |f, i| {
        try writer.writeAll(f.name);
        if (i != fields.len - 1) try writer.writeAll(",");
    }
}

pub fn renderError(self: Self, err: Error, writer: anytype, term: std.io.tty.Config, file_path: ?[]const u8) !void {
    const loc = self.tokenLoc(err.token);
    const loc_extra = loc.extraInfo(self.source);

    // 'file:line:column error: MSG'
    try term.setColor(writer, .bold);
    try writer.print("{?s}:{d}:{d}\n", .{ file_path, loc_extra.line, loc_extra.col });
    try term.setColor(writer, .reset);
    try term.setColor(writer, .dim);
    try writer.print("{d} │ ", .{loc_extra.line});
    try term.setColor(writer, .reset);
    try writer.writeAll(self.source[loc_extra.line_start..loc.start]);
    try term.setColor(writer, .green);
    try writer.writeAll(self.source[loc.start..loc.end]);
    try term.setColor(writer, .reset);
    try writer.writeAll(self.source[loc.end..loc_extra.line_end]);
    try writer.writeByte('\n');

    // location pointer
    const line_number_len = (std.math.log10(loc_extra.line) + 1) + 3;
    try writer.writeByteNTimes(
        ' ',
        line_number_len + (loc_extra.col - 1),
    );
    try term.setColor(writer, .bold);
    try term.setColor(writer, .red);
    try writer.writeByte('^');
    if (loc.end > loc.start) try writer.writeByteNTimes('~', loc.end - loc.start - 1);
    try writer.writeByte(' ');
    try switch (err.tag) {
        .expected_token => writer.print("expected {s}", .{err.expected_tag.?.symbol()}),
        .expected_unary_expr => writer.writeAll("expected unary expression"),
        .expected_expr => writer.writeAll("expected expression"),
        .expected_lhs_expr => writer.writeAll("expects left hand side expression"),
        .expected_case_selector => writer.writeAll("expected case selector"),
        .expected_struct_member => writer.writeAll("expected struct member"),
        .expected_function_parameter => writer.writeAll("expected function parameter"),
        .expected_global_decl => writer.writeAll("expected global declaration"),
        .expected_global_directive => writer.writeAll("expected global directive"),
        .expected_compound_statement => writer.writeAll("expected compound statement"),
        .expected_continuing_compound_statement => writer.writeAll("expected continuing compound statement"),
        .expected_continuing_statement => writer.writeAll("expected continuing statement"),
        .expected_statement => writer.writeAll("expected statement"),
        .expected_type_specifier => writer.writeAll("expected type specifier"),
        .invalid_attribute => {
            try writer.writeAll("invalid attribute");
            try renderList(writer, Node.Attribute);
        },
        .invalid_attributes => writer.writeAll("unexpected attribute list"),
        .invalid_element_count => writer.writeAll("invalid element count"),
        .invalid_initializer => writer.writeAll("variable requires a type or initializer"),
        .invalid_assignment_op => writer.writeAll("invalid assignment op"),
        .invalid_severity => {
            try writer.writeAll("invalid severity");
            try renderList(writer, Node.Severity);
        },
        .type_needs_ext => writer.writeAll("type requires an extension"),
        .empty_struct => writer.writeAll("empty structs are forbidden"),
        .deep_template => writer.writeAll("template too deep"),
        .unresolved_module => writer.writeAll("could not resolve module"),
    };

    try term.setColor(writer, .reset);
    try writer.writeByte('\n');
}
