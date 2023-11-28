const std = @import("std");

pub const Node = union(enum) {
    pub const Index = u32;
    pub const ExtraIndex = u32;
    pub const IdentIndex = u32;
    pub const Tag = std.meta.FieldEnum(Self);

    const Self = @This();
    pub const Error = struct {
        tag: u8,
        expected_token_tag: u8 = 0,
        source_info: ExtraIndex,
    };
    /// For storing lists of nodes
    pub const Span = struct {
        from: Index,
        to: Index,

        pub fn orderedRemove(self: *@This(), arr: []Node.Index, i: usize) void {
            for (self.from + i..self.to - 1) |j| {
                arr[j] = arr[j + 1];
            }
            self.to -= 1;
        }
    };
    pub const Comment = struct { ident: IdentIndex };
    pub const Ident = struct {
        name: IdentIndex,
        /// 0 means empty
        template_list: Index,
    };
    pub const Let = struct { typed_ident: ExtraIndex, initializer: Index };
    pub const SingleExpr = struct { expr: Index };
    pub const Assign = struct { lhs_expr: Index, rhs_expr: Index };
    pub const ShiftExpr = struct { lhs_unary_expr: Index, rhs_unary_expr: Index };
    pub const RelationalExpr = struct { lhs_shift_expr: Index, rhs_shift_expr: Index };
    pub const MultiplicativeExpr = struct { lhs_multiplicative_expr: Index, rhs_unary_expr: Index };
    pub const AdditiveExpr = struct { lhs_additive_expr: Index, rhs_mul_expr: Index };
    pub const ShortCircuitExpr = struct { lhs_relational_expr: Index, rhs_relational_expr: Index };
    pub const BitwiseExpr = struct { lhs_bitwise_expr: Index, rhs_unary_expr: Index };

    pub const n_directive_tags = 4;
    pub const DiagnosticDirective = struct { diagnostic_control: ExtraIndex };
    pub const GlobalVar = struct { global_var: ExtraIndex, initializer: Index };
    pub const Override = struct { override: ExtraIndex, initializer: Index };
    pub const Import = struct { aliases: Index, module: IdentIndex };

    pub const IdentList = struct { idents: Index };
    pub const ImportAlias = struct {
        old: IdentIndex,
        /// 0 means no alias
        new: IdentIndex,
    };

    pub const Fn = struct { fn_header: ExtraIndex, body: Index };
    pub const TypeAlias = struct { new_name: IdentIndex, old_type: Index };
    pub const Struct = struct { name: IdentIndex, members: Index };
    pub const StructMember = struct { attributes: Index, typed_ident: ExtraIndex };
    pub const FnParam = struct { attributes: Index, fn_param: ExtraIndex };
    pub const Loop = struct { attributes: Index, body: Index };
    pub const Compound = struct { attributes: Index, statements: Index };
    pub const For = struct { for_header: ExtraIndex, body: Index };
    pub const If = struct { condition: Index, body: Index };
    pub const Switch = struct { expr: Index, body: Index };
    pub const While = struct { condition: Index, body: Index };
    pub const Return = struct {
        /// 0 means no expression
        expr: Index,
    };
    pub const Call = struct {
        ident: Index,
        /// 0 means no arguments
        arguments: Index,
    };
    pub const Var = struct { @"var": ExtraIndex, initializer: Index };
    pub const Continuing = struct { body: Index };
    pub const BreakIf = struct { condition: Index };
    pub const Else = struct { @"if": Index, body: Index };
    pub const ElseIf = struct { if1: Index, if2: Index };
    pub const SwitchBody = struct { attributes: Index, clauses: Index };
    pub const CaseClause = struct { selectors: Index, body: Index };
    pub const CaseSelector = struct {
        /// 0 means `default`
        expr: Index,
    };
    pub const FieldAccess = struct { lhs_expr: Index, member: IdentIndex };
    pub const IndexAccess = struct { lhs_expr: Index, index_expr: Index };
    pub const Number = struct { value: IdentIndex };

    // Util
    @"error": Error,
    span: Span,
    comment: Comment,
    // Directives
    diagnostic_directive: DiagnosticDirective,
    enable_directive: IdentList,
    requires_directive: IdentList,
    import: Import,
    import_alias: ImportAlias,
    // Global declarations
    global_var: Self.GlobalVar,
    override: Self.Override,
    @"fn": Fn,
    fn_param: Self.FnParam,
    @"const": Let,
    type_alias: TypeAlias,
    @"struct": Struct,
    struct_member: StructMember,
    // Global declaration helpers
    attribute: Attribute,
    ident: Ident,
    type: Ident,
    // Statements
    loop: Loop,
    compound: Compound,
    @"for": For,
    @"if": If,
    @"else": Else,
    else_if: ElseIf,
    @"switch": Switch,
    switch_body: SwitchBody,
    case_clause: CaseClause,
    case_selector: CaseSelector,
    @"while": While,
    @"return": Return,
    call: Call,
    // variable_or_value_statement
    @"var": Self.Var,
    let: Let,
    // const
    @"break": void,
    @"continue": void,
    discard: void,
    // There's plenty of space in this tag to list all operators.
    increment: SingleExpr,
    decrement: SingleExpr,
    phony_assign: SingleExpr,
    @"=": Assign,
    @"+=": Assign,
    @"-=": Assign,
    @"*=": Assign,
    @"/=": Assign,
    @"%=": Assign,
    @"&=": Assign,
    @"|=": Assign,
    @"^=": Assign,
    @"<<=": Assign,
    @">>=": Assign,
    const_assert: SingleExpr, // also a global declaration
    continuing: Continuing, // Loop helper
    break_if: BreakIf, // Continuing helper
    // Expressions
    // https://www.w3.org/TR/WGSL/#operator-precedence-associativity
    paren: SingleExpr,
    logical_not: SingleExpr,
    bitwise_complement: SingleExpr,
    negative: SingleExpr,
    deref: SingleExpr,
    ref: SingleExpr,
    lshift: ShiftExpr,
    rshift: ShiftExpr,
    lt: RelationalExpr,
    gt: RelationalExpr,
    lte: RelationalExpr,
    gte: RelationalExpr,
    eq: RelationalExpr,
    neq: RelationalExpr,
    mul: MultiplicativeExpr,
    div: MultiplicativeExpr,
    mod: MultiplicativeExpr,
    add: AdditiveExpr,
    sub: AdditiveExpr,
    logical_and: ShortCircuitExpr,
    logical_or: ShortCircuitExpr,
    bitwise_and: BitwiseExpr,
    bitwise_or: BitwiseExpr,
    bitwise_xor: BitwiseExpr,
    field_access: FieldAccess,
    index_access: IndexAccess,
    // Literals
    true: void,
    false: void,
    number: Number,

    comptime {
        // Size will be 9 in MultiArrayList because tag will shrink from 4 bytes to 1
        std.debug.assert(@sizeOf(Self) == 12);
    }
};

// For parsing
pub const Attribute = union(enum) {
    pub const Tag = std.meta.FieldEnum(@This());
    pub const SingleExpr = Node.Index;
    pub const DiagnosticControl = Node.ExtraIndex;
    pub const Interpolate = Node.ExtraIndex;
    pub const WorkgroupSize = Node.ExtraIndex;
    const Self = @This();

    compute: void,
    @"const": void,
    fragment: void,
    invariant: void,
    must_use: void,
    vertex: void,
    @"align": SingleExpr,
    binding: SingleExpr,
    builtin: SingleExpr,
    group: SingleExpr,
    id: SingleExpr,
    location: SingleExpr,
    size: SingleExpr,
    diagnostic: Self.DiagnosticControl,
    interpolate: Interpolate,
    workgroup_size: Self.WorkgroupSize,

    comptime {
        std.debug.assert(@sizeOf(Self) == 8);
    }
};
pub const Severity = enum(Node.Index) {
    @"error",
    warning,
    info,
    off,
};

// For ast.extraData
pub const GlobalVar = struct {
    attrs: Node.Index = 0,
    name: Node.IdentIndex,
    template_list: Node.Index = 0,
    type: Node.Index = 0,
};
pub const Var = struct {
    name: Node.IdentIndex,
    template_list: Node.Index = 0,
    type: Node.Index = 0,
};
pub const TypedIdent = struct {
    name: Node.IdentIndex,
    type: Node.Index,
};
pub const Override = struct {
    attrs: Node.Index = 0,
    name: Node.IdentIndex,
    type: Node.Index = 0,
};
pub const WorkgroupSize = struct {
    x: Node.Index,
    y: Node.Index = 0,
    z: Node.Index = 0,
};
pub const FnParam = struct {
    name: Node.IdentIndex,
    type: Node.Index = 0,
};
pub const FnHeader = struct {
    attrs: Node.Index = 0,
    name: Node.IdentIndex,
    params: Node.Index = 0,
    return_attrs: Node.Index = 0,
    return_type: Node.Index = 0,
};
pub const ForHeader = struct {
    attrs: Node.Index = 0,
    init: Node.Index = 0,
    cond: Node.Index = 0,
    update: Node.Index = 0,
};
pub const DiagnosticControl = struct {
    severity: Node.Index,
    name: Node.IdentIndex,
    field: Node.IdentIndex = 0,
};
pub const Interpolation = struct {
    type: Node.Index,
    sampling_expr: Node.Index = 0,
};
pub const SourceInfo = struct {
    line: Node.IdentIndex,
    line_num: u32,
    col_num: u32,
    tok_len: u32,
};
