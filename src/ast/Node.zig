const std = @import("std");
const Loc = @import("../file/Loc.zig");

pub const ExtraIndex = Loc.Index;
pub const IdentIndex = Loc.Index;
pub const Index = Loc.Index;

/// Used for error messages
src_offset: Loc.Index = 0,
/// Determines what is stored in lhs and rhs.
tag: Tag,
/// Opaque type. See Parser for details.
lhs: Index = 0,
/// Opaque type. See Parser for details. Parser guarantees to come from token AFTER lhs.
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
};

pub const GlobalVar = struct {
    attrs: Index = 0,
    name: IdentIndex,
    address_space: Index = 0,
    access_mode: Index = @intFromEnum(AccessMode.read),
    type: Index = 0,
};
pub const Var = struct {
    name: IdentIndex,
    address_space: Index = 0,
    access_mode: Index = @intFromEnum(AccessMode.read),
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

/// https://www.w3.org/TR/WGSL/#address-space
pub const AddressSpace = enum(Loc.Index) {
    function = 1,
    private,
    workgroup,
    uniform,
    storage,
    handle,
};
pub const AccessMode = enum(Loc.Index) { read, write, read_write };

/// https://www.w3.org/TR/WGSL/#builtin-inputs-outputs
pub const Builtin = enum(Loc.Index) {
    vertex_index,
    instance_index,
    position,
    fragment,
    front_facing,
    frag_depth,
    sample_index,
    sample_mask,
    local_invocation_id,
    local_invocation_index,
    global_invocation_id,
    workgroup_id,
    num_workgroups,
};

/// https://www.w3.org/TR/WGSL/#interpolation
pub const InterpolationType = enum(Loc.Index) { perspective, linear, flat };
pub const InterpolationSampling = enum(Loc.Index) { center = 1, centroid, sample };

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
