const std = @import("std");
const Token = @import("Token.zig");

pub const Index = u32;
pub const ExtraIndex = u32;
pub const Tag = Data.Tag;

tag: Tag,
/// For error reporting, identifiers, and operators.
/// For spans stores the token that starts the list. May be one of
token: Token.Index,
// Can't use a tagged union because this goes into a MultiArrayList which will use 7 more bytes
// per Node.
data: Data,

pub const Data = union {
    const Self = @This();
    pub const Tag = std.meta.FieldEnum(Self);

    pub const Span = struct { from: Index, to: Index };
    pub const DiagnosticDirective = struct { control: ExtraIndex };
    pub const EnableDirective = struct { ident_tokens: Index };
    pub const RequiresDirective = struct { ident_tokens: Index };
    pub const GlobalVar = struct { global_var: ExtraIndex, initializer: Index };
    pub const Override = struct { override: ExtraIndex, initializer: Index };
    pub const Fn = struct { fn_header: ExtraIndex, body: Index };
    pub const Const = struct { type: Index, initializer: Index };
    pub const TypeAlias = struct { value: Index };
    pub const Import = struct { aliases: Index, module: Token.Index };
    pub const Struct = struct { members: Index };
    pub const Attr = struct { tag: Attribute.Tag, data: Attribute };
    pub const Ident = struct {
        /// 0 means empty
        template_list: Index,
    };
    pub const Type = struct {
        /// 0 means empty
        template_list: Index,
    };
    pub const Alias = struct { alias: Token.Index };
    pub const StructMember = struct { attributes: Index, type: Index };
    pub const FnParam = struct { attributes: Index, type: Index };
    pub const Loop = struct { attributes: Index, body: Index };
    pub const Compound = struct { attributes: Index, statements: Index };
    pub const For = struct { for_header: ExtraIndex, body: Index };
    pub const If = struct { condition: Index, body: Index };
    pub const Switch = struct { expr: Index, body: Index };
    pub const While = struct { condition: Index, body: Index };
    pub const Return = struct {
        /// 0 means no return expression
        expr: Index,
    };
    pub const Call = struct {
        ident: Index,
        /// 0 means no arguments
        arguments: Index,
    };
    pub const Var = struct { @"var": ExtraIndex, initializer: Index };
    pub const Let = struct { type: Index, initializer: Index };
    pub const VariableUpdating = struct {
        /// 0 means '_'
        lhs_expr: Index,
        rhs_expr: Index,
    };
    pub const ConstAssert = struct { expr: Index };
    pub const Continuing = struct { body: Index };
    pub const BreakIf = struct { condition: Index };
    pub const Else = struct { @"if": Index, body: Index };
    pub const ElseIf = struct { if1: Index, if2: Index };
    pub const SwitchBody = struct { attributes: Index, clauses: Index };
    pub const CaseClause = struct { selectors: Index, body: Index };
    pub const CaseSelector = struct {
        /// 0 means `default`
        expression: Index,
    };
    pub const ParenExpr = struct { expr: Index };
    pub const UnaryExpr = struct { expr: Index };
    pub const ShiftExpr = struct { lhs_unary_expr: Index, rhs_unary_expr: Index };
    pub const RelationalExpr = struct { lhs_shift_expr: Index, rhs_shift_expr: Index };
    pub const MultiplicativeExpr = struct { lhs_multiplicative_expr: Index, rhs_unary_expr: Index };
    pub const AdditiveExpr = struct { lhs_additive_expr: Index, rhs_mul_expr: Index };
    pub const ShortCircuitExpr = struct { lhs_relational_expr: Index, rhs_relational_expr: Index };
    pub const BitwiseExpr = struct { lhs_bitwise_expr: Index, rhs_unary_expr: Index };
    pub const LhsExpr = struct { lhs_expr: Index };
    pub const FieldAccess = struct { lhs_expr: Index, member: Token.Index };
    pub const IndexAccess = struct { lhs_expr: Index, index_expr: Index };

    // These can't be void because of how Parser.addNode loops through field types to find field names.
    pub const Break = struct {};
    pub const Continue = struct {};
    pub const Discard = struct {};
    pub const True = struct {};
    pub const False = struct {};
    pub const Number = struct {};
    // Util
    empty: void, // For removing nodes in-place
    span: Span, // For storing lists of nodes
    // Directives
    diagnostic_directive: DiagnosticDirective,
    enable_directive: EnableDirective,
    requires_directive: RequiresDirective,
    // Global declarations
    global_var: Self.GlobalVar,
    override: Self.Override,
    @"fn": Fn,
    @"const": Const,
    type_alias: TypeAlias,
    // const_assert_statement
    import: Import,
    @"struct": Struct,
    // Global declaration helpers
    attr: Attr,
    ident: Ident,
    type: Type,
    alias: Alias, // Import helper
    struct_member: StructMember,
    fn_param: FnParam,
    // Statements
    loop: Loop,
    compound: Compound,
    @"for": For,
    @"if": If,
    @"switch": Switch,
    @"while": While,
    @"return": Return,
    call: Call,
    // variable_or_value_statement
    @"var": Self.Var,
    let: Let,
    // const
    @"break": Break,
    @"continue": Continue,
    discard: Discard,
    variable_updating: VariableUpdating,
    const_assert: ConstAssert,
    continuing: Continuing, // Loop helper
    break_if: BreakIf, // Continuing helper
    // Statement helpers
    @"else": Else,
    else_if: ElseIf,
    switch_body: SwitchBody,
    case_clause: CaseClause,
    case_selector: CaseSelector,
    // Expressions
    paren_expr: ParenExpr,
    /// main_token is one of @"!", .@"~", .@"-", .@"*", .@"&"
    unary_expr: UnaryExpr,
    /// main_token is one of .@"<<", .@">>"
    shift_expr: ShiftExpr,
    /// main_token is one of .@"<", .@">", .@"<=", .@">=", .@"==", .@"!="
    relational_expr: RelationalExpr,
    /// main_token is one of .@"*", .@"/", .@"%"
    multiplicative_expr: MultiplicativeExpr,
    /// main_token is one of .@"+", .@"-"
    additive_expr: AdditiveExpr,
    /// main_token is one of @"&&", @"||"
    short_circuit_expr: ShortCircuitExpr,
    /// main_token is one of .@"&", .@"|", .@"^"
    bitwise_expr: BitwiseExpr,
    /// main_token is one of .@"*" (deref), .@"&" (addr_of)
    lhs_expr: LhsExpr,
    // Expression helpers
    field_access: FieldAccess,
    index_access: IndexAccess,
    // Literals
    true: True,
    false: False,
    number: Number,

    comptime {
        std.debug.assert(@sizeOf(@This()) <= 4 * @sizeOf(u32));
    }
};

// For parsing
pub const Attribute = union {
    pub const Tag = std.meta.FieldEnum(@This());
    pub const SingleExpr = Index;
    pub const DiagnosticControl = ExtraIndex;
    pub const Interpolate = ExtraIndex;
    pub const WorkgroupSize = ExtraIndex;

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
    diagnostic: Attribute.DiagnosticControl,
    interpolate: Interpolate,
    workgroup_size: Attribute.WorkgroupSize,
};
pub const Severity = enum(Index) {
    @"error",
    warning,
    info,
    off,
};

// For ast.extraData
pub const GlobalVar = struct {
    attrs: Index = 0,
    name: Token.Index,
    template_list: Index = 0,
    type: Index = 0,
};
pub const Var = struct {
    name: Token.Index,
    template_list: Index = 0,
    type: Index = 0,
};
pub const Override = struct {
    attrs: Index = 0,
    type: Index = 0,
};
pub const WorkgroupSize = struct {
    x: Index,
    y: Index = 0,
    z: Index = 0,
};
pub const FnHeader = struct {
    attrs: Index = 0,
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
pub const DiagnosticControl = struct {
    severity: Token.Index,
    name: Token.Index,
    field: Token.Index = 0,
};
pub const Interpolation = struct {
    type: Index,
    sampling_expr: Index = 0,
};
