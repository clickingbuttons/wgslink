const std = @import("std");
const Token = @import("./Token.zig");
const Node = @import("../ast/Node.zig");

pub const Tag = enum(u8) {
    deep_template,
    empty_struct,
    expected_attribute_decl,
    expected_attribute_statement,
    expected_case_selector,
    expected_compound_statement,
    expected_continuing_compound_statement,
    expected_continuing_statement,
    expected_expr,
    expected_function_parameter,
    expected_global_decl,
    expected_global_directive,
    expected_lhs_expr,
    expected_statement,
    expected_struct_member,
    expected_template_elaborated_ident,
    expected_token,
    expected_type_specifier,
    expected_unary_expr,
    invalid_assignment_op,
    invalid_attribute,
    invalid_element_count,
    invalid_initializer,
    invalid_severity,
    type_needs_ext,
    unresolved_module,
};

pub fn render(
    writer: anytype,
    term: std.io.tty.Config,
    file_path: ?[]const u8,
    tag: Tag,
    expected_tag: Token.Tag,
    line: []const u8,
    source_info: Node.SourceInfo,
) !void {
    const line_num = source_info.line_num;
    const token_start = source_info.col_num - 1;
    const token_end = token_start + source_info.tok_len;
    // 'file:line:column error: MSG'
    try term.setColor(writer, .bold);
    if (file_path) |f| try writer.print("{s}:", .{f});
    try writer.print("{d}:{d}\n", .{ line_num, token_start });
    try term.setColor(writer, .reset);
    try term.setColor(writer, .dim);
    try writer.print("{d} │ ", .{line_num});
    try term.setColor(writer, .reset);
    try writer.writeAll(line[0..token_start]);
    try term.setColor(writer, .green);
    try writer.writeAll(line[token_start..token_end]);
    try term.setColor(writer, .reset);
    try writer.writeAll(line[token_end..]);
    try writer.writeByte('\n');

    const line_number_len = (std.math.log10(line_num) + 1) + 3;
    try writer.writeByteNTimes(' ', line_number_len + token_start);
    try term.setColor(writer, .bold);
    try term.setColor(writer, .red);
    try writer.writeByte('^');
    try writer.writeByte(' ');
    try switch (tag) {
        .deep_template => writer.writeAll("template too deep"),
        .empty_struct => writer.writeAll("empty structs are forbidden"),
        .expected_attribute_decl => writer.writeAll("expected global declaration with attributes like a variable, override, or function declaration"),
        .expected_attribute_statement => writer.writeAll("expected statement with attributes like a loop, compound, or for statement"),
        .expected_case_selector => writer.writeAll("expected case selector"),
        .expected_compound_statement => writer.writeAll("expected compound statement"),
        .expected_continuing_compound_statement => writer.writeAll("expected continuing compound statement"),
        .expected_continuing_statement => writer.writeAll("expected continuing statement"),
        .expected_expr => writer.writeAll("expected expression"),
        .expected_function_parameter => writer.writeAll("expected function parameter"),
        .expected_global_decl => writer.writeAll("expected global declaration"),
        .expected_global_directive => writer.writeAll("expected global directive"),
        .expected_lhs_expr => writer.writeAll("expects left hand side expression"),
        .expected_statement => writer.writeAll("expected statement"),
        .expected_struct_member => writer.writeAll("expected struct member"),
        .expected_template_elaborated_ident => writer.writeAll("expected identifier with optional template"),
        .expected_token => writer.print("expected {s}", .{expected_tag.symbol()}),
        .expected_type_specifier => writer.writeAll("expected type specifier"),
        .expected_unary_expr => writer.writeAll("expected unary expression"),
        .invalid_assignment_op => writer.writeAll("invalid assignment op"),
        .invalid_attribute => {
            try writer.writeAll("invalid attribute");
            try renderList(writer, Node.Attribute.Tag);
        },
        .invalid_element_count => writer.writeAll("invalid element count"),
        .invalid_initializer => writer.writeAll("variable requires a type or initializer"),
        .invalid_severity => {
            try writer.writeAll("invalid severity");
            try renderList(writer, Node.Severity);
        },
        .type_needs_ext => writer.writeAll("type requires an extension"),
        .unresolved_module => writer.writeAll("could not resolve module"),
    };

    try term.setColor(writer, .reset);
    try writer.writeByte('\n');
}

fn renderList(writer: anytype, @"enum": anytype) !void {
    try writer.writeAll(". expected one of ");
    const fields = @typeInfo(@"enum").Enum.fields;
    inline for (fields, 0..) |f, i| {
        try writer.writeAll(f.name);
        if (i != fields.len - 1) try writer.writeAll(",");
    }
}
