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
    abstract_int,
    i32,
    u32,
    abstract_float,
    f32,
    f16,
};

const Self = @This();

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

/// Copy-paste of std.builtin.type + Matrix
pub const Type = union(enum) {
    Bool: void,
    Int: Int,
    Float: Float,
    Array: Array,
    Struct: Struct,
    Vector: Vector,
    Matrix: Matrix,

    pub const Signedness = enum {
        signed,
        unsigned,
    };

    pub const Int = struct {
        signedness: Signedness,
        bits: u16,
    };

    pub const Float = struct {
        bits: u16,
    };

    pub const Pointer = struct {
        size: Size,
        address_space: AddressSpace,
        child: type,

        pub const Size = enum(u2) {
            One,
            Many,
            Slice,
            C,
        };
    };

    pub const Array = struct {
        len: comptime_int,
        child: type,

        /// The type of the sentinel is the element type of the array, which is
        /// the value of the `child` field in this struct. However there is no way
        /// to refer to that type here, so we use pointer to `anyopaque`.
        sentinel: ?*const anyopaque,
    };

    pub const StructField = struct {
        name: []const u8,
        type: type,
        default_value: ?*const anyopaque,
        is_comptime: bool,
        alignment: comptime_int,
    };

    pub const Struct = struct {
        fields: []const StructField,
    };

    pub const Vector = struct {
        len: comptime_int,
        child: type,
    };

    pub const Matrix = struct {
        width: comptime_int,
        height: comptime_int,
        child: type,
    };
};

pub const Var = struct {
    /// Only global variables can have this.
    attrs: Index = 0,
    name: IdentIndex,
    address_space: AddressSpace,
    access_mode: AccessMode = .read,
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
/// https://www.w3.org/TR/WGSL/#interpolation
pub const Interpolation = struct {
    pub const Type = enum { perspective, linear, flat };
    pub const Sampling = enum { center, centroid, sample };

    type: Interpolation.Type = .perspective,
    sampling: Sampling = .center,
};

/// https://www.w3.org/TR/WGSL/#address-space
pub const AddressSpace = enum {
    function,
    private,
    workgroup,
    uniform,
    storage,
    handle,
};
pub const AccessMode = enum { read, write, read_write };

/// https://www.w3.org/TR/WGSL/#builtin-inputs-outputs
pub const Builtin = enum {
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

pub const DiagnosticControl = struct {
    pub const Severity = enum { @"error", warning, info, off };
    severity: Severity,
    name: IdentIndex,
    field: IdentIndex = 0,
};
