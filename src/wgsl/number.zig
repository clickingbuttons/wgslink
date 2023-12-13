const std = @import("std");
const Loc = @import("../file/Loc.zig");

pub const Base = enum(u8) { decimal = 10, hex = 16 };
pub const FloatBase = enum(u8) { decimal = 10, hex = 16 };

pub const Result = union(enum) {
    abstract_int: i64,
    i32: i32,
    u32: u32,
    abstract_float: f64,
    f32: f32,
    f16: f16,
    failure: Error,
};

pub const Error = union(enum) {
    leading_zero,
    expected_digit_after_base,
    /// Invalid digit for the specified base.
    invalid_digit: struct { i: Loc.Index, base: Base },
    /// Float literal has multiple periods.
    duplicate_period,
    /// Float literal has multiple exponents.
    duplicate_exponent: Loc.Index,
    /// Number ends in special character (+-.)
    trailing_special: Loc.Index,
    /// Character not in [0-9a-zA-Z.+-]
    invalid_character: Loc.Index,
    /// [+-] not immediately after [pPeE]
    invalid_exponent_sign: Loc.Index,
    /// Integer outside of i64, i32, or u32 range
    overflow,
    /// [iu] after a float literal
    invalid_suffix,
    /// characters after suffix
    trailing_chars: Loc.Index,
};

pub fn parse(bytes: []const u8) !Result {
    var i: Loc.Index = 0;
    var base: u8 = 10;
    var leading_zero = false;
    if (bytes.len >= 2 and bytes[0] == '0') switch (bytes[1]) {
        'x', 'X' => {
            base = 16;
            i = 2;
        },
        '.', 'e', 'E', 'i', 'u', 'f', 'h' => {},
        else => leading_zero = true,
    };
    if (bytes.len == 2 and base != 10) return .{ .failure = .expected_digit_after_base };

    var x: i64 = 0;
    var overflow = false;
    var period = false;
    var special: u8 = 0;
    var exponent = false;
    var float = false;
    while (i < bytes.len) : (i += 1) {
        const c = bytes[i];
        switch (c) {
            'e', 'E' => if (base == 10) {
                float = true;
                if (exponent) return .{ .failure = .{ .duplicate_exponent = i } };
                special = c;
                exponent = true;
                continue;
            },
            'p', 'P' => if (base == 16) {
                float = true;
                if (exponent) return .{ .failure = .{ .duplicate_exponent = i } };
                special = c;
                exponent = true;
                continue;
            },
            '.' => {
                float = true;
                if (period) return .{ .failure = .duplicate_period };
                period = true;
                special = c;
                continue;
            },
            '+', '-' => {
                switch (special) {
                    'p', 'P' => {},
                    'e', 'E' => if (base != 10) return .{ .failure = .{ .invalid_exponent_sign = i } },
                    else => return .{ .failure = .{ .invalid_exponent_sign = i } },
                }
                special = c;
                continue;
            },
            'i', 'u' => {
                special = c;
                break;
            },
            'f', 'h' => {
                if (base == 10 or exponent) {
                    special = c;
                    float = true;
                    break;
                }
            },
            else => {},
        }
        const digit = switch (c) {
            '0'...'9' => c - '0',
            'A'...'Z' => c - 'A' + 10,
            'a'...'z' => c - 'a' + 10,
            else => return .{ .failure = .{ .invalid_character = i } },
        };
        if (digit >= base) return .{ .failure = .{ .invalid_digit = .{ .i = i, .base = @as(Base, @enumFromInt(base)) } } };
        special = 0;

        if (float) continue;
        if (x != 0) {
            const res = @mulWithOverflow(x, base);
            if (res[1] != 0) overflow = true;
            x = res[0];
        }
        const res = @addWithOverflow(x, digit);
        if (res[1] != 0) overflow = true;
        x = res[0];
    }
    switch (special) {
        'i', 'u', 'f', 'h' => {
            if (bytes.len > i + 1) return .{ .failure = .{ .trailing_chars = i } };
        },
        else => {},
    }
    switch (special) {
        0 => {},
        inline 'i', 'u' => |t| {
            if (float) return .{ .failure = .invalid_suffix };
            if (leading_zero) return .{ .failure = .leading_zero };
            const T = if (t == 'i') i32 else u32;
            if (std.math.cast(T, x)) |res| {
                return if (T == i32) .{ .i32 = res } else .{ .u32 = res };
            } else {
                overflow = true;
            }
        },
        inline 'f', 'h' => |t| {
            const T = if (t == 'f') f32 else f16;
            const res = try std.fmt.parseFloat(T, bytes[0 .. bytes.len - 1]);
            return if (T == f32) .{ .f32 = res } else .{ .f16 = res };
        },
        '.' => {},
        else => return .{ .failure = .{ .trailing_special = i } },
    }

    if (float) {
        const res = try std.fmt.parseFloat(f64, bytes);
        return .{ .abstract_float = res };
    }
    if (leading_zero) return .{ .failure = .leading_zero };
    if (overflow) return .{ .failure = .overflow };
    return .{ .abstract_int = x };
}

fn testParse(bytes: []const u8, expected: Result) !void {
    const parsed = try parse(bytes);
    try std.testing.expectEqual(expected, parsed);
}

test "decimal integer literals" {
    try testParse("0", .{ .abstract_int = 0 });
    try testParse("01", .{ .failure = .{ .leading_zero = {} } });
    try testParse("01x", .{ .failure = .{ .invalid_digit = .{ .i = 2, .base = .decimal } } });
    try testParse("123", .{ .abstract_int = 123 });
    try testParse("1u", .{ .u32 = 1 });
    try testParse("0i", .{ .i32 = 0 });
}

test "hexadecimal integer literals" {
    try testParse("0x123", .{ .abstract_int = 0x123 });
    try testParse("0x12fe", .{ .abstract_int = 0x12fe });
    try testParse("0X12fu", .{ .u32 = 0x12f });
    try testParse("0X12fi", .{ .i32 = 0x12f });
    try testParse("0x123456789i", .{ .failure = .overflow });
}

test "decimal float literals" {
    try testParse("01.", .{ .abstract_float = 1.0 });
    try testParse(".01", .{ .abstract_float = 0.01 });
    try testParse("12.34", .{ .abstract_float = 12.34 });
    try testParse(".0f", .{ .f32 = 0.0 });
    try testParse("0h", .{ .f16 = 0.0 });
    try testParse("1e-3", .{ .abstract_float = 1e-3 });
    try testParse("0.e+4f", .{ .f32 = 0.e+4 });
}

test "decimal hex literals" {
    try testParse("0xa.fp+2", .{ .abstract_float = 0xa.fp+2 });
    try testParse("0x1P+4f", .{ .f32 = 0x1p+4 });
    try testParse("0X.3", .{ .abstract_float = 0x.3 });
    try testParse("0x3p+2h", .{ .f16 = 0x3p+2 });
    try testParse("0X1.fp-4", .{ .abstract_float = 0x1.fp-4 });
    try testParse("0x3.2p+2h", .{ .f16 = 0x3.2p+2 });
    try testParse("0x3.2p+2h1", .{ .failure = .{ .trailing_chars = 8 } });
}
