const std = @import("std");
const Token = @import("Token.zig");

// pub const Index = union {
//     token: Token.Index,
//     node: u31,
//     extra: u31,
//
//     comptime {
//         std.debug.assert(@bitSizeOf(@This()) == @sizeOf(u31));
//     }
// };
pub const Index = u32;

tag: Tag,
// For error reporting and rendering
main_token: Token.Index,
lhs: Index = 0,
rhs: Index = 0,

pub const Attribute = enum(u32) {
    @"align",
    binding,
    builtin,
    @"const",
    compute,
    diagnostic,
    fragment,
    group,
    id,
    interpolate,
    invariant,
    location,
    must_use,
    size,
    vertex,
    workgroup_size,
};

pub const Severity = enum(u32) {
    @"error",
    warning,
    info,
    off,
};

pub const Tag = enum {
    empty,
    span,
    global_var,
    override,
    type_alias,
    const_assert,
    @"struct",
    struct_member,
    @"fn",
    fn_param,
    compound_statement,
    @"return",
    discard,
    loop,
    continuing,
    break_if,
    @"break",
    @"continue",
    @"if",
    @"else",
    else_if,
    @"switch",
    switch_body,
    case_clause,
    case_selector,
    @"var",
    @"const",
    let,
    @"while",
    @"for",
    increase,
    decrease,
    compound_assign,
    phony_assign,
    number_type,
    bool_type,
    sampler_type,
    vector_type,
    matrix_type,
    atomic_type,
    array_type,
    ptr_type,
    sampled_texture_type,
    multisampled_texture_type,
    external_texture_type,
    storage_texture_type,
    depth_texture_type,
    attribute,
    mul,
    div,
    mod,
    add,
    sub,
    shl,
    shr,
    @"and",
    @"or",
    xor,
    logical_and,
    logical_or,
    not,
    negate,
    deref,
    addr_of,
    equal,
    not_equal,
    less_than,
    less_than_equal,
    greater_than,
    greater_than_equal,
    call,
    ident,
    field_access,
    index_access,
    true,
    false,
    number,
    comment,
    paren_expr,
    diagnostic_directive,
    enable_directive,
    requires_directive,
    import,
    alias,

    comptime {
        // Goal is to keep this under one byte for efficiency.
        std.debug.assert(@sizeOf(@This()) == 1);
    }
};

pub const GlobalVar = struct {
    attrs: Index = 0,
    name: Token.Index,
    template_list: Token.Index = 0,
    type: Index = 0,
};

pub const Var = struct {
    name: Token.Index,
    template_list: Token.Index = 0,
    type: Index = 0,
};

pub const Override = struct {
    attrs: Index = 0,
    type: Index = 0,
};

pub const PtrType = struct {
    addr_space: Token.Index,
    access_mode: Token.Index,
};

pub const WorkgroupSize = struct {
    x: Index,
    y: Index = 0,
    z: Index = 0,
};

pub const FnProto = struct {
    attrs: Index = 0,
    params: Index = 0,
    return_attrs: Index = 0,
    return_type: Index = 0,
};

pub const ForHeader = struct {
    attrs: Index,
    init: Index,
    cond: Index,
    update: Index,
};

pub const DiagnosticControl = struct {
    severity: Index,
    name: Index,
    field: Index = 0,
};

pub const Interpolation = struct {
    type: Index,
    sampling: Index = 0,
};
