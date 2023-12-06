const std = @import("std");
const Loc = @import("../file/Loc.zig");

tag: Tag,
loc: Loc,

pub const Tag = enum(u8) {
    invalid = 0,
    eof,
    ident,
    number,
    template_args_start,
    template_args_end,
    string_literal,
    @"(",
    @")",
    @"{",
    @"}",
    @"[",
    @"]",
    @".",
    @",",
    @":",
    @";",
    @"->",
    @"@",
    @"=",
    @"==",
    @"!",
    @"!=",
    @"&",
    @"&=",
    @"&&",
    @"|",
    @"|=",
    @"||",
    @"~",
    @"+",
    @"+=",
    @"++",
    @"-",
    @"-=",
    @"--",
    @"*",
    @"*=",
    @"/",
    @"/=",
    @"%",
    @"%=",
    @"^",
    @"^=",
    @"<",
    @"<=",
    @"<<",
    @"<<=",
    @">",
    @">=",
    @">>",
    @">>=",
    @"_",
    // For errors
    @"\n",

    k_alias,
    k_break,
    k_case,
    k_const,
    k_const_assert,
    k_continue,
    k_continuing,
    k_default,
    k_diagnostic,
    k_discard,
    k_else,
    k_enable,
    k_false,
    k_fn,
    k_for,
    k_if,
    k_let,
    k_loop,
    k_override,
    k_requires,
    k_return,
    k_struct,
    k_switch,
    k_true,
    k_var,
    k_while,
    // Not part of spec.
    k_import,
    k_as,
    k_from,

    const Self = @This();

    pub fn symbol(self: Self) []const u8 {
        return switch (self) {
            .eof => "EOF",
            .invalid => "invalid bytes",
            .ident => "an identifier",
            .number => "a number literal",
            .string_literal => "a string literal",
            .template_args_start => "<",
            .template_args_end => ">",

            .@"(",
            .@")",
            .@"{",
            .@"}",
            .@"[",
            .@"]",
            .@".",
            .@",",
            .@":",
            .@";",
            .@"->",
            .@"@",
            .@"=",
            .@"==",
            .@"!",
            .@"!=",
            .@"&",
            .@"&=",
            .@"&&",
            .@"|",
            .@"|=",
            .@"||",
            .@"~",
            .@"+",
            .@"+=",
            .@"++",
            .@"-",
            .@"-=",
            .@"--",
            .@"*",
            .@"*=",
            .@"/",
            .@"/=",
            .@"%",
            .@"%=",
            .@"^",
            .@"^=",
            .@"<",
            .@"<=",
            .@"<<",
            .@"<<=",
            .@">",
            .@">=",
            .@">>",
            .@">>=",
            ._,
            => |t| @tagName(t),

            .k_import => "// import",
            else => |t| @tagName(t)["k_".len..],
        };
    }
};

pub const keywords = brk: {
    @setEvalBranchQuota(4000);
    const KV = struct { []const u8, Tag };
    var kvs: [100]KV = undefined;
    var i: usize = 0;
    for (@typeInfo(Tag).Enum.fields) |f| {
        if (f.name.len >= 2 and std.mem.eql(u8, f.name[0..2], "k_")) {
            kvs[i] = KV{ f.name[2..], @enumFromInt(f.value) };
            i += 1;
        }
    }
    break :brk std.ComptimeStringMap(Tag, kvs[0..i]);
};

pub const reserved = blk: {
    @setEvalBranchQuota(4000);
    break :blk std.ComptimeStringMap(void, .{
        .{"NULL"},
        .{"Self"},
        .{"abstract"},
        .{"active"},
        .{"alignas"},
        .{"alignof"},
        .{"as"},
        .{"asm"},
        .{"asm_fragment"},
        .{"async"},
        .{"attribute"},
        .{"auto"},
        .{"await"},
        .{"become"},
        .{"binding_array"},
        .{"cast"},
        .{"catch"},
        .{"class"},
        .{"co_await"},
        .{"co_return"},
        .{"co_yield"},
        .{"coherent"},
        .{"column_major"},
        .{"common"},
        .{"compile"},
        .{"compile_fragment"},
        .{"concept"},
        .{"const_cast"},
        .{"consteval"},
        .{"constexpr"},
        .{"constinit"},
        .{"crate"},
        .{"debugger"},
        .{"decltype"},
        .{"delete"},
        .{"demote"},
        .{"demote_to_helper"},
        .{"do"},
        .{"dynamic_cast"},
        .{"enum"},
        .{"explicit"},
        .{"export"},
        .{"extends"},
        .{"extern"},
        .{"external"},
        .{"fallthrough"},
        .{"filter"},
        .{"final"},
        .{"finally"},
        .{"friend"},
        .{"from"},
        .{"fxgroup"},
        .{"get"},
        .{"goto"},
        .{"groupshared"},
        .{"highp"},
        .{"impl"},
        .{"implements"},
        .{"import"},
        .{"inline"},
        .{"instanceof"},
        .{"interface"},
        .{"layout"},
        .{"lowp"},
        .{"macro"},
        .{"macro_rules"},
        .{"match"},
        .{"mediump"},
        .{"meta"},
        .{"mod"},
        .{"module"},
        .{"move"},
        .{"mut"},
        .{"mutable"},
        .{"namespace"},
        .{"new"},
        .{"nil"},
        .{"noexcept"},
        .{"noinline"},
        .{"nointerpolation"},
        .{"noperspective"},
        .{"null"},
        .{"nullptr"},
        .{"of"},
        .{"operator"},
        .{"package"},
        .{"packoffset"},
        .{"partition"},
        .{"pass"},
        .{"patch"},
        .{"pixelfragment"},
        .{"precise"},
        .{"precision"},
        .{"premerge"},
        .{"priv"},
        .{"protected"},
        .{"pub"},
        .{"public"},
        .{"readonly"},
        .{"ref"},
        .{"regardless"},
        .{"register"},
        .{"reinterpret_cast"},
        .{"require"},
        .{"resource"},
        .{"restrict"},
        .{"self"},
        .{"set"},
        .{"shared"},
        .{"sizeof"},
        .{"smooth"},
        .{"snorm"},
        .{"static"},
        .{"static_assert"},
        .{"static_cast"},
        .{"std"},
        .{"subroutine"},
        .{"super"},
        .{"target"},
        .{"template"},
        .{"this"},
        .{"thread_local"},
        .{"throw"},
        .{"trait"},
        .{"try"},
        .{"type"},
        .{"typedef"},
        .{"typeid"},
        .{"typename"},
        .{"typeof"},
        .{"union"},
        .{"unless"},
        .{"unorm"},
        .{"unsafe"},
        .{"unsized"},
        .{"use"},
        .{"using"},
        .{"varying"},
        .{"virtual"},
        .{"volatile"},
        .{"wgsl"},
        .{"where"},
        .{"with"},
        .{"writeonly"},
        .{"yield"},
    });
};

pub const builtins = blk: {
    @setEvalBranchQuota(8000);
    break :blk std.ComptimeStringMap(void, .{
        // Value Constructors
        .{"array"},
        .{"bool"},
        .{"f16"},
        .{"f32"},
        .{"i32"},
        .{"u32"},
        .{"mat2x2"},
        .{"mat2x3"},
        .{"mat2x4"},
        .{"mat3x2"},
        .{"mat3x3"},
        .{"mat3x4"},
        .{"mat4x2"},
        .{"mat4x3"},
        .{"mat4x4"},
        .{"mat2x2f"},
        .{"mat2x3f"},
        .{"mat2x4f"},
        .{"mat3x2f"},
        .{"mat3x3f"},
        .{"mat3x4f"},
        .{"mat4x2f"},
        .{"mat4x3f"},
        .{"mat4x4f"},
        .{"mat2x2h"},
        .{"mat2x3h"},
        .{"mat2x4h"},
        .{"mat3x2h"},
        .{"mat3x3h"},
        .{"mat3x4h"},
        .{"mat4x2h"},
        .{"mat4x3h"},
        .{"mat4x4h"},
        .{"mat2x2i"},
        .{"mat2x3i"},
        .{"mat2x4i"},
        .{"mat3x2i"},
        .{"mat3x3i"},
        .{"mat3x4i"},
        .{"mat4x2i"},
        .{"mat4x3i"},
        .{"mat4x4i"},
        .{"mat2x2u"},
        .{"mat2x3u"},
        .{"mat2x4u"},
        .{"mat3x2u"},
        .{"mat3x3u"},
        .{"mat3x4u"},
        .{"mat4x2u"},
        .{"mat4x3u"},
        .{"mat4x4u"},
        // Structures
        .{"u32"},
        .{"vec2"},
        .{"vec3"},
        .{"vec4"},
        .{"vec2f"},
        .{"vec3f"},
        .{"vec4f"},
        .{"vec2h"},
        .{"vec3h"},
        .{"vec4h"},
        .{"vec2i"},
        .{"vec3i"},
        .{"vec4i"},
        .{"vec2u"},
        .{"vec3u"},
        .{"vec4u"},
        // Bit Reinterpretation Built-in Functions
        .{"bitcast"},
        // Logical Built-in Functions
        .{"all"},
        .{"any"},
        .{"select"},
        // Array Built-in Functions
        .{"arrayLength"},
        // Numeric Built-in Functions
        .{"abs"},
        .{"acos"},
        .{"acosh"},
        .{"asin"},
        .{"asinh"},
        .{"atan"},
        .{"atanh"},
        .{"atan2"},
        .{"ceil"},
        .{"clamp"},
        .{"cos"},
        .{"cosh"},
        .{"countLeadingZeros"},
        .{"countOneBits"},
        .{"countTrailingZeros"},
        .{"cross"},
        .{"degrees"},
        .{"determinant"},
        .{"distance"},
        .{"dot"},
        .{"dot4U8Packed"},
        .{"dot4I8Packed"},
        .{"exp"},
        .{"exp2"},
        .{"extractBits"},
        .{"faceForward"},
        .{"firstLeadingBit"},
        .{"firstTrailingBit"},
        .{"floor"},
        .{"fma"},
        .{"fract"},
        .{"frexp"},
        .{"insertBits"},
        .{"inverseSqrt"},
        .{"ldexp"},
        .{"length"},
        .{"log"},
        .{"log2"},
        .{"max"},
        .{"min"},
        .{"mix"},
        .{"modf"},
        .{"normalize"},
        .{"pow"},
        .{"quantizeToF16"},
        .{"radians"},
        .{"reflect"},
        .{"refract"},
        .{"reverseBits"},
        .{"round"},
        .{"saturate"},
        .{"sign"},
        .{"sin"},
        .{"sinh"},
        .{"smoothstep"},
        .{"sqrt"},
        .{"step"},
        .{"tan"},
        .{"tanh"},
        .{"transpose"},
        .{"trunc"},
        // Derivative Built-in Functions
        .{"dpdx"},
        .{"dpdxCoarse"},
        .{"dpdxFine"},
        .{"dpdy"},
        .{"dpdyCoarse"},
        .{"dpdyFine"},
        .{"fwidth"},
        .{"fwidthCoarse"},
        .{"fwidthFine"},
        // Texture Built-in Functions
        .{"textureDimensions"},
        .{"textureGather"},
        .{"textureGatherCompare"},
        .{"textureLoad"},
        .{"textureNumLayers"},
        .{"textureNumLevels"},
        .{"textureNumSamples"},
        .{"textureSample"},
        .{"textureSampleBias"},
        .{"textureSampleCompare"},
        .{"textureSampleCompareLevel"},
        .{"textureSampleGrad"},
        .{"textureSampleLevel"},
        .{"textureSampleBaseClampToEdge"},
        .{"textureStore"},
        // Atomic Built-in Functions
        .{"atomicLoad"},
        .{"atomicStore"},
        .{"atomicAdd"},
        .{"atomicAdd"},
        .{"atomicSub"},
        .{"atomicMax"},
        .{"atomicMin"},
        .{"atomicAnd"},
        .{"atomicOr"},
        .{"atomicXor"},
        .{"atomicExchange"},
        .{"atomicCompareExchangeWeak"},
        // Data Packing Built-in Functions
        .{"pack4x8snorm"},
        .{"pack4x8unorm"},
        .{"pack4xI8"},
        .{"pack4xU8"},
        .{"pack4xI8Clamp"},
        .{"pack4xU8Clamp"},
        .{"pack2x16snorm"},
        .{"pack2x16unorm"},
        .{"pack2x16float"},
        // Data Unpacking Built-in Functions
        .{"unpack4x8snorm"},
        .{"unpack4x8unorm"},
        .{"unpack4xI8"},
        .{"unpack4xU8"},
        .{"unpack2x16snorm"},
        .{"unpack2x16unorm"},
        .{"unpack2x16float"},
        // Synchronization Built-in Functions
        .{"storageBarrier"},
        .{"textureBarrier"},
        .{"workgroupBarrier"},
        .{"workgroupUniformLoad"},
    });
};
