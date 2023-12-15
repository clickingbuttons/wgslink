const std = @import("std");
const Token = @import("./Token.zig");
const Node = @import("../ast/Node.zig");
const Loc = @import("../file/Loc.zig");

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
    invalid_address_space,
    invalid_access_mode,
    invalid_builtin,
    invalid_interpolation_type,
    invalid_interpolation_sampling,
    type_needs_ext,

    // See number.Error
    leading_zero,
    expected_digit_after_base,
    invalid_digit,
    duplicate_period,
    duplicate_exponent,
    trailing_special,
    invalid_character,
    invalid_exponent_sign,
    overflow,
    invalid_suffix,
    trailing_chars,

    pub fn render(
        self: @This(),
        writer: anytype,
        extra: Loc.Index,
    ) !void {
        try switch (self) {
            .deep_template => writer.writeAll("template too deep"),
            .empty_struct => writer.writeAll("empty structs are forbidden"),
            .expected_attribute_decl => writer.writeAll("expected global declaration with attributes (variable, override, function)"),
            .expected_attribute_statement => writer.writeAll("expected statement with attributes (loop, compound, for)"),
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
            .expected_token => {
                const expected_tag: Token.Tag = @enumFromInt(extra);
                try writer.print("expected {s}", .{expected_tag.symbol()});
            },
            .expected_type_specifier => writer.writeAll("expected type specifier"),
            .expected_unary_expr => writer.writeAll("expected unary expression"),
            .invalid_assignment_op => writer.writeAll("invalid assignment op"),
            .invalid_attribute => {
                try writer.writeAll("invalid attribute");
                try renderList(writer, Node.Attribute);
            },
            .invalid_element_count => writer.writeAll("invalid element count"),
            .invalid_initializer => writer.writeAll("variable requires a type or initializer"),
            .invalid_severity => {
                try writer.writeAll("invalid severity");
                try renderList(writer, Node.DiagnosticControl.Severity);
            },
            .invalid_address_space => {
                try writer.writeAll("invalid address space");
                try renderList(writer, Node.AddressSpace);
            },
            .invalid_access_mode => {
                try writer.writeAll("invalid access mode");
                try renderList(writer, Node.AccessMode);
            },
            .invalid_builtin => {
                try writer.writeAll("invalid builtin");
                try renderList(writer, Node.Attribute.Builtin);
            },
            .invalid_interpolation_type => {
                try writer.writeAll("invalid interpolation type");
                try renderList(writer, Node.Interpolation.Type);
            },
            .invalid_interpolation_sampling => {
                try writer.writeAll("invalid interpolation sampling");
                try renderList(writer, Node.Interpolation.Sampling);
            },
            .type_needs_ext => writer.writeAll("type requires an extension directive at the top of the program"),
            .leading_zero => writer.writeAll("unexpected leading 0"),
            .expected_digit_after_base => writer.writeAll("expected digit after base specifier"),
            .invalid_digit => writer.print("invalid digit for base {d}", .{extra}),
            .duplicate_period => writer.writeAll("duplicate decimal point"),
            .duplicate_exponent => writer.writeAll("duplicate exponent"),
            .trailing_special => writer.writeAll("unexpected trailing character after float literal"),
            .invalid_character => writer.writeAll("invalid character"),
            .invalid_exponent_sign => writer.writeAll("[+-] must be immediately after [pPeE]"),
            .overflow => writer.writeAll("cannot be exactly represented as a float literal"),
            .invalid_suffix => writer.writeAll("unexpected suffix after float literal"),
            .trailing_chars => writer.writeAll("unexpected characters after float literal suffix"),
        };
    }
};

fn renderList(writer: anytype, @"enum": anytype) !void {
    try writer.writeAll(". expected one of ");
    const fields = @typeInfo(@"enum").Enum.fields;
    inline for (fields, 0..) |f, i| {
        try writer.writeAll(f.name);
        if (i != fields.len - 1) try writer.writeAll(",");
    }
}
