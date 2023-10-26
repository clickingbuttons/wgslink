const std = @import("std");
const Token = @import("Token.zig");

const Tokenizer = @This();

source: [:0]const u8,
index: u32 = 0,

const State = union(enum) {
    start,
    ident,
    underscore,
    number: struct {
        is_hex: bool = false,
        allow_leading_sign: bool = false,
        has_dot: bool = false,
    },
    line_comment,
    block_comment,
    block_comment_ending,
    ampersand,
    bang,
    equal,
    angle_bracket_left,
    angle_bracket_angle_bracket_left,
    angle_bracket_right,
    angle_bracket_angle_bracket_right,
    minus,
    percent,
    dot,
    pipe,
    plus,
    slash,
    asterisk,
    xor,
};

pub fn init(source: [:0]const u8) Tokenizer {
    // skip the UTF-8 BOM if present
    const src_start: u32 = if (std.mem.startsWith(u8, source, "\xEF\xBB\xBF")) 3 else 0;
    return Tokenizer{ .source = source[src_start..] };
}

pub fn peek(self: *Tokenizer) Token {
    var index = self.index;
    var state: State = .start;
    var result = Token{
        .tag = .eof,
        .loc = .{
            .start = index,
            .end = undefined,
        },
    };

    while (true) : (index += 1) {
        var c = self.source[index];
        switch (state) {
            .start => switch (c) {
                0 => {
                    if (index != self.source.len) {
                        result.tag = .invalid;
                        index += 1;
                    }
                    break;
                },
                ' ', '\n', '\t', '\r' => result.loc.start = index + 1,

                'a'...'z', 'A'...'Z' => state = .ident,
                '0'...'9' => state = .{ .number = .{} },
                '&' => state = .ampersand,
                '!' => state = .bang,
                '=' => state = .equal,
                '<' => state = .angle_bracket_left,
                '>' => state = .angle_bracket_right,
                '-' => state = .minus,
                '%' => state = .percent,
                '.' => state = .dot,
                '|' => state = .pipe,
                '+' => state = .plus,
                '/' => state = .slash,
                '*' => state = .asterisk,
                '_' => state = .underscore,
                '^' => state = .xor,

                '@' => {
                    result.tag = .attr;
                    index += 1;
                    break;
                },
                '[' => {
                    result.tag = .bracket_left;
                    index += 1;
                    break;
                },
                ']' => {
                    result.tag = .bracket_right;
                    index += 1;
                    break;
                },
                '{' => {
                    result.tag = .brace_left;
                    index += 1;
                    break;
                },
                '}' => {
                    result.tag = .brace_right;
                    index += 1;
                    break;
                },
                ':' => {
                    result.tag = .colon;
                    index += 1;
                    break;
                },
                ',' => {
                    result.tag = .comma;
                    index += 1;
                    break;
                },
                '(' => {
                    result.tag = .paren_left;
                    index += 1;
                    break;
                },
                ')' => {
                    result.tag = .paren_right;
                    index += 1;
                    break;
                },
                ';' => {
                    result.tag = .semicolon;
                    index += 1;
                    break;
                },
                '~' => {
                    result.tag = .tilde;
                    index += 1;
                    break;
                },

                else => {
                    result.tag = .invalid;
                    index += 1;
                    break;
                },
            },
            .ident => switch (c) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => {},
                else => {
                    result.tag = .ident;
                    if (Token.keywords.get(self.source[result.loc.start..index])) |tag| {
                        result.tag = tag;
                    } else if (Token.reserved.get(self.source[result.loc.start..index])) |_| {
                        result.tag = .invalid;
                    }
                    break;
                },
            },
            .underscore => switch (c) { // TODO: two underscore `__` https://www.w3.org/TR/WGSL/#identifiers
                'a'...'z', 'A'...'Z', '_', '0'...'9' => state = .ident,
                else => {
                    result.tag = .underscore;
                    break;
                },
            },
            .number => |*number| {
                result.tag = .number;
                switch (c) {
                    '0'...'9' => {},
                    'a'...'d', 'A'...'D' => if (!number.is_hex) break,
                    'x', 'X' => number.is_hex = true,
                    '.' => {
                        if (number.has_dot) break;
                        number.has_dot = true;
                    },
                    '+', '-' => {
                        if (!number.allow_leading_sign) break;
                        number.allow_leading_sign = false;
                        number.is_hex = false;
                    },
                    'e', 'E' => if (!number.is_hex) {
                        number.allow_leading_sign = true;
                    },
                    'p', 'P' => if (number.is_hex) {
                        number.allow_leading_sign = true;
                    },
                    'i', 'u' => {
                        index += 1;
                        break;
                    },
                    'f', 'h' => if (!number.is_hex) {
                        index += 1;
                        break;
                    },
                    else => break,
                }
            },
            .line_comment => switch (c) {
                0 => break,
                '\n' => {
                    result.tag = .line_comment;
                    break;
                },
                else => {},
            },
            .block_comment => switch (c) {
                '*' => {
                    state = .block_comment_ending;
                },
                else => {},
            },
            .block_comment_ending => switch (c) {
                '/' => {
                    result.tag = .block_comment;
                    index += 1;
                    break;
                },
                else => {
                    state = .block_comment;
                }
            },
            .ampersand => switch (c) {
                '&' => {
                    result.tag = .ampersand_ampersand;
                    index += 1;
                    break;
                },
                '=' => {
                    result.tag = .ampersand_equal;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .ampersand;
                    break;
                },
            },
            .bang => switch (c) {
                '=' => {
                    result.tag = .bang_equal;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .bang;
                    break;
                },
            },
            .equal => switch (c) {
                '=' => {
                    result.tag = .equal_equal;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .equal;
                    break;
                },
            },
            .angle_bracket_left => switch (c) {
                '<' => state = .angle_bracket_angle_bracket_left,
                '=' => {
                    result.tag = .angle_bracket_left_equal;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .angle_bracket_left;
                    break;
                },
            },
            .angle_bracket_angle_bracket_left => switch (c) {
                '=' => {
                    result.tag = .angle_bracket_angle_bracket_left_equal;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .angle_bracket_angle_bracket_left;
                    break;
                },
            },
            .angle_bracket_right => switch (c) {
                '>' => state = .angle_bracket_angle_bracket_right,
                '=' => {
                    result.tag = .angle_bracket_right_equal;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .angle_bracket_right;
                    break;
                },
            },
            .angle_bracket_angle_bracket_right => switch (c) {
                '=' => {
                    result.tag = .angle_bracket_angle_bracket_right_equal;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .angle_bracket_angle_bracket_right;
                    break;
                },
            },
            .minus => switch (c) {
                '-' => {
                    result.tag = .minus_minus;
                    index += 1;
                    break;
                },
                '=' => {
                    result.tag = .minus_equal;
                    index += 1;
                    break;
                },
                '>' => {
                    result.tag = .arrow;
                    index += 1;
                    break;
                },
                '0'...'9' => {
                    // workaround for x-1 being tokenized as [x] [-1]
                    // TODO: maybe it's user fault? :^)
                    // duplicated at .plus too
                    if (index >= 2 and std.ascii.isAlphabetic(self.source[index - 2])) {
                        result.tag = .minus;
                        break;
                    }
                    state = .{ .number = .{} };
                },
                else => {
                    result.tag = .minus;
                    break;
                },
            },
            .percent => switch (c) {
                '=' => {
                    result.tag = .percent_equal;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .percent;
                    break;
                },
            },
            .pipe => switch (c) {
                '|' => {
                    result.tag = .pipe_pipe;
                    index += 1;
                    break;
                },
                '=' => {
                    result.tag = .pipe_equal;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .pipe;
                    break;
                },
            },
            .dot => switch (c) {
                '0'...'9' => state = .{ .number = .{} },
                else => {
                    result.tag = .dot;
                    break;
                },
            },
            .plus => switch (c) {
                '+' => {
                    result.tag = .plus_plus;
                    index += 1;
                    break;
                },
                '=' => {
                    result.tag = .plus_equal;
                    index += 1;
                    break;
                },
                '0'...'9' => {
                    if (index >= 2 and std.ascii.isAlphabetic(self.source[index - 2])) {
                        result.tag = .plus;
                        break;
                    }
                    state = .{ .number = .{} };
                },
                else => {
                    result.tag = .plus;
                    break;
                },
            },
            .slash => switch (c) {
                '/' => state = .line_comment,
                '*' => state = .block_comment,
                '=' => {
                    result.tag = .slash_equal;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .slash;
                    break;
                },
            },
            .asterisk => switch (c) {
                '=' => {
                    result.tag = .asterisk_equal;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .asterisk;
                    break;
                },
            },
            .xor => switch (c) {
                '=' => {
                    result.tag = .xor_equal;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .xor;
                    break;
                },
            },
        }
    }

    result.loc.end = index;
    return result;
}

pub fn next(self: *Tokenizer) Token {
    const tok = self.peek();
    self.index = tok.loc.end;
    return tok;
}

test "tokenize identifier and numbers and comments" {
    const str =
        \\_ __ _iden iden -100i 100.8i // cc
        \\// comment
        \\/*
        \\ block*comment
        \\ */
        \\
    ;
    var tokenizer = Tokenizer.init(str);
    const Tag = Token.Tag;
    try std.testing.expectEqual(Tag.underscore, tokenizer.next().tag);
    try std.testing.expectEqual(Tag.ident, tokenizer.next().tag);
    try std.testing.expectEqual(Tag.ident, tokenizer.next().tag);
    try std.testing.expectEqual(Tag.ident, tokenizer.next().tag);
    const int1 = tokenizer.next();
    try std.testing.expectEqual(Tag.number, int1.tag);
    try std.testing.expectEqualStrings("-100i", int1.loc.slice(str));
    const int2 = tokenizer.next();
    try std.testing.expectEqual(Tag.number, int2.tag);
    try std.testing.expectEqualStrings("100.8i", int2.loc.slice(str));
    const comment1 = tokenizer.next();
    try std.testing.expectEqual(Tag.line_comment, comment1.tag);
    try std.testing.expectEqualStrings("// cc", comment1.loc.slice(str));
    const comment2 = tokenizer.next();
    try std.testing.expectEqual(Tag.line_comment, comment2.tag);
    try std.testing.expectEqualStrings("// comment", comment2.loc.slice(str));
    const comment3 = tokenizer.next();
    try std.testing.expectEqual(Tag.block_comment, comment3.tag);
    try std.testing.expectEqualStrings("/*\n block*comment\n */", comment3.loc.slice(str));
    try std.testing.expectEqual(Tag.eof, tokenizer.next().tag);
}
