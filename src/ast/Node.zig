const std = @import("std");
const Loc = @import("../file/Loc.zig");
pub const ErrorLoc = @import("../file/Error.zig").ErrorLoc;

pub const IndexTag = enum {
    node,
    ident,
    extra,
    @"opaque",
    empty,
};
pub const ExtraIndex = Loc.Index;
pub const IdentIndex = Loc.Index;
pub const Index = Loc.Index;

/// Used for error messages
src_offset: Loc.Index = 0,
/// Determines what is stored in lhs and rhs.
tag: Tag,
/// Opaque type. See Tag.lhsTag for IndexTag.
lhs: Index = 0,
/// Opaque type. See Tag.rhsTag for IndexTag. Parser guarantees to come from token AFTER lhs.
rhs: Index = 0,

pub const Tag = enum(u8) {
    pub const n_directives = 4;
    // Directives
    diagnostic_directive,
    enable_directive,
    requires_directive,
    import,
    import_alias, // import helper
    // Util
    @"error",
    span,
    comment,
    // Global declarations
    global_var,
    override,
    @"fn",
    fn_param,
    @"const",
    type_alias,
    @"struct",
    struct_member,
    const_assert, // also a statement
    // Global declaration helpers
    attribute,
    ident,
    type,
    // Statements
    loop,
    continuing, // Loop helper
    break_if, // Continuing helper
    compound,
    @"for",
    @"if",
    @"else",
    else_if,
    @"switch",
    switch_body,
    case_clause,
    case_selector,
    @"while",
    @"return",
    call,
    @"var",
    let,
    // const
    @"break",
    @"continue",
    discard,
    // There's plenty of space in this tag to list all operators.
    increment,
    decrement,
    phony_assign,
    @"=",
    @"+=",
    @"-=",
    @"*=",
    @"/=",
    @"%=",
    @"&=",
    @"|=",
    @"^=",
    @"<<=",
    @">>=",
    // Expressions
    // https://www.w3.org/TR/WGSL/#operator-precedence-associativity
    // Unary
    paren,
    logical_not,
    bitwise_complement,
    negative,
    deref,
    ref,
    // Non-unary
    lshift,
    rshift,
    lt,
    gt,
    lte,
    gte,
    eq,
    neq,
    mul,
    div,
    mod,
    add,
    sub,
    logical_and,
    logical_or,
    bitwise_and,
    bitwise_or,
    bitwise_xor,
    field_access,
    index_access,
    // Literals
    true,
    false,
    number,
};

const Self = @This();

pub fn lhsTag(self: Self) IndexTag {
    return switch (self) {
        .@"error",
        .attribute,
        .span,
        => .@"opaque",
        .true, .false, .number => .empty,
        .comment, .ident, .import_alias, .type_alias, .@"struct", .number => .ident,
        .let,
        .@"const",
        .diagnostic_directive,
        .global_var,
        .override,
        .@"fn",
        .@"for",
        .@"var",
        => .extra,
        else => .node,
    };
}

pub fn rhsTag(self: Self) IndexTag {
    return switch (self) {
        .@"error" => .tag,
        .import, .import_alias, .field_access => .ident,
        .struct_member, .fn_param => .extra,
        .let,
        .@"const",
        .@"=",
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
        .index_access,
        => .node,
        .attribute => {
            const att: Attribute = @enumFromInt(self.lhs);
            return att.indexTag();
        },
        else => .empty,
    };
}

// pub fn orderedRemove(self: *@This(), arr: []Node.Index, i: usize) void {
//     for (self.from + i..self.to - 1) |j| {
//         arr[j] = arr[j + 1];
//     }
//     self.to -= 1;
// }

// For parsing
pub const Attribute = enum(Loc.Index) {
    compute,
    @"const",
    fragment,
    invariant,
    must_use,
    vertex,
    @"align",
    binding,
    builtin,
    group,
    id,
    location,
    size,
    diagnostic,
    interpolate,
    workgroup_size,

    pub fn indexTag(self: @This()) IndexTag {
        switch (self) {
            .compute, .@"const", .fragment, .invariant, .must_use, .vertex => .empty,
            .@"align", .binding, .builtin, .group, .id, .location, .size => .node,
            .diagnostic, .interpolate, .workgroup_size => .extra,
        }
    }
};

pub const GlobalVar = struct {
    attrs: Index = 0,
    name: IdentIndex,
    template_list: Index = 0,
    type: Index = 0,
};
pub const Var = struct {
    name: IdentIndex,
    template_list: Index = 0,
    type: Index = 0,
};
pub const TypedIdent = struct {
    name: IdentIndex,
    type: Index,
};
pub const Override = struct {
    attrs: Index = 0,
    name: IdentIndex,
    type: Index = 0,
};
pub const WorkgroupSize = struct {
    x: Index,
    y: Index = 0,
    z: Index = 0,
};
pub const FnParam = struct {
    name: IdentIndex,
    type: Index = 0,
};
pub const FnHeader = struct {
    attrs: Index = 0,
    name: IdentIndex,
    params: Index = 0,
    return_attrs: Index = 0,
    return_type: Index = 0,
};
pub const ForHeader = struct {
    attrs: Index = 0,
    init: Index = 0,
    cond: Index = 0,
    update: Index = 0,
};
pub const Interpolation = struct {
    type: Index,
    sampling_expr: Index = 0,
};

pub const Severity = enum(Loc.Index) {
    @"error",
    warning,
    info,
    off,
};
pub const DiagnosticControl = struct {
    severity: Index,
    name: IdentIndex,
    field: IdentIndex = 0,
};
