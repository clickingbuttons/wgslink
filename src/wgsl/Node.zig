const std = @import("std");
const Token = @import("Token.zig");

tag: Tag,
main_token: Token.Index,
lhs: Index = 0,
rhs: Index = 0,

pub const Tag = enum {
    span,
    global_var,
    override,
    type_alias,
    const_assert,
    @"struct",
    struct_member,
    @"fn",
    fn_param,
    block,
    @"return",
    discard,
    loop,
    continuing,
    break_if,
    @"break",
    @"continue",
    @"if",
    if_else,
    if_else_if,
    @"switch",
    switch_case,
    switch_default,
    switch_case_default,
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
    attr_const,
    attr_invariant,
    attr_must_use,
    attr_vertex,
    attr_fragment,
    attr_compute,
    attr_align,
    attr_binding,
    attr_group,
    attr_id,
    attr_location,
    attr_size,
    attr_builtin,
    attr_workgroup_size,
    attr_interpolate,
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
    bitcast,
    ident,
    field_access,
    index_access,
    true,
    false,
    number,
    comment,
    paren_expr,
    diagnostic,
    enable,
    requires,
    import,

    comptime {
        // Goal is to keep this under one byte for efficiency.
        std.debug.assert(@sizeOf(@This()) == 1);
    }
};

pub const Index = u32;

pub const GlobalVar = struct {
    attrs: Index = 0,
    name: Token.Index,
    addr_space: Token.Index = 0,
    access_mode: Token.Index = 0,
    type: Index = 0,
};

pub const Var = struct {
    name: Token.Index,
    addr_space: Token.Index = 0,
    access_mode: Token.Index = 0,
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
    init: Index = 0,
    cond: Index = 0,
    update: Index = 0,
};

pub const DiagnosticRule = struct {
    name: Index,
    field: Index = 0,
};

pub const Builtin = enum {
    vertex_index,
    instance_index,
    position,
    front_facing,
    frag_depth,
    local_invocation_id,
    local_invocation_index,
    global_invocation_id,
    workgroup_id,
    num_workgroups,
    sample_index,
    sample_mask,
};

pub const InterpolationType = enum {
    perspective,
    linear,
    flat,
};

pub const InterpolationSample = enum {
    center,
    centroid,
    sample,
};

pub const AddressSpace = enum {
    function,
    private,
    workgroup,
    uniform,
    storage,
    handle,
};

pub const AccessMode = enum {
    read,
    write,
    read_write,
};

pub const Attribute = enum {
    invariant,
    @"const",
    must_use,
    vertex,
    fragment,
    compute,
    @"align",
    binding,
    group,
    id,
    location,
    size,
    builtin,
    workgroup_size,
    interpolate,
};

pub const TexelFormat = enum {
    rgba8unorm,
    rgba8snorm,
    rgba8uint,
    rgba8sint,
    rgba16uint,
    rgba16sint,
    rgba16float,
    r32uint,
    r32sint,
    r32float,
    rg32uint,
    rg32sint,
    rg32float,
    rgba32uint,
    rgba32sint,
    rgba32float,
    bgra8unorm,
};

pub const Extensions = struct {
    f16: bool = false,
};

pub const Severity = enum {
    @"error",
    warning,
    info,
    off
};
