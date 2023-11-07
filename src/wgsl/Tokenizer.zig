const std = @import("std");
const Token = @import("Token.zig");

pub const Tokenizer = struct {
    buffer: [:0]const u8,
    index: u32,

    pub fn init(buffer: [:0]const u8) Tokenizer {
        // skip UTF-8 BOM
        const src_start: u32 = if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0;
        return Tokenizer{
            .buffer = buffer,
            .index = src_start,
        };
    }

    // For debugging
    pub fn dump(self: *Tokenizer, token: Token) void {
        std.debug.print("{s} \"{s}\"\n", .{ @tagName(token.tag), self.buffer[token.loc.start..token.loc.end] });
    }

    pub fn peek(self: *Tokenizer) Token {
        const State = union(enum) {
            start,
            ident,
            number: struct {
                is_hex: bool = false,
                allow_leading_sign: bool = false,
                has_attr: bool = false,
            },
            line_comment,
            block_comment,
            block_comment_ending,
            @"&",
            @"!",
            @"=",
            @"<",
            @"<<",
            @">",
            @">>",
            @"-",
            @"%",
            @".",
            @"|",
            @"+",
            @"/",
            @"*",
            @"^",
            _,
            @"'",
            @"\"",
        };
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
            var c = self.buffer[index];
            switch (state) {
                .start => switch (c) {
                    0 => {
                        if (index != self.buffer.len) {
                            result.tag = .invalid;
                            index += 1;
                        }
                        break;
                    },
                    ' ', '\n', '\t', '\r' => result.loc.start = index + 1,

                    'a'...'z', 'A'...'Z' => state = .ident,
                    '0'...'9' => state = .{ .number = .{} },
                    '&' => state = .@"&",
                    '!' => state = .@"!",
                    '=' => state = .@"=",
                    '<' => state = .@"<",
                    '>' => state = .@">",
                    '-' => state = .@"-",
                    '%' => state = .@"%",
                    '.' => state = .@".",
                    '|' => state = .@"|",
                    '+' => state = .@"+",
                    '/' => state = .@"/",
                    '*' => state = .@"*",
                    '_' => state = ._,
                    '^' => state = .@"^",
                    '\'' => state = .@"'",
                    '"' => state = .@"\"",

                    '@' => {
                        result.tag = .@"@";
                        index += 1;
                        break;
                    },
                    '[' => {
                        result.tag = .@"[";
                        index += 1;
                        break;
                    },
                    ']' => {
                        result.tag = .@"]";
                        index += 1;
                        break;
                    },
                    '{' => {
                        result.tag = .@"{";
                        index += 1;
                        break;
                    },
                    '}' => {
                        result.tag = .@"}";
                        index += 1;
                        break;
                    },
                    ':' => {
                        result.tag = .@":";
                        index += 1;
                        break;
                    },
                    ',' => {
                        result.tag = .@",";
                        index += 1;
                        break;
                    },
                    '(' => {
                        result.tag = .@"(";
                        index += 1;
                        break;
                    },
                    ')' => {
                        result.tag = .@")";
                        index += 1;
                        break;
                    },
                    ';' => {
                        result.tag = .@";";
                        index += 1;
                        break;
                    },
                    '~' => {
                        result.tag = .@"~";
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
                        if (Token.keywords.get(self.buffer[result.loc.start..index])) |tag| {
                            result.tag = tag;
                        } else if (Token.reserved.get(self.buffer[result.loc.start..index])) |_| {
                            result.tag = .invalid;
                        }
                        break;
                    },
                },
                ._ => switch (c) {
                    'a'...'z', 'A'...'Z', '0'...'9' => state = .ident,
                    '_' => {
                        result.tag = .invalid;
                        index += 1;
                        break;
                    },
                    else => {
                        result.tag = ._;
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
                            if (number.has_attr) break;
                            number.has_attr = true;
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
                    0 => {
                        result.loc.start = index;
                        if (index != self.buffer.len) {
                            result.tag = .invalid;
                            index += 1;
                        }
                        break;
                    },
                    't' => {
                        if (std.mem.eql(u8, Token.Tag.k_import.symbol(), self.buffer[result.loc.start .. index + 1])) {
                            result.tag = .k_import;
                            index += 1;
                            break;
                        }
                    },
                    '\n' => {
                        result.loc.start = index + 1;
                        state = .start;
                    },
                    else => {},
                },
                .block_comment => switch (c) {
                    '*' => state = .block_comment_ending,
                    else => {},
                },
                .block_comment_ending => switch (c) {
                    '/' => state = .start,
                    else => state = .block_comment,
                },
                .@"&" => switch (c) {
                    '&' => {
                        result.tag = .@"&&";
                        index += 1;
                        break;
                    },
                    '=' => {
                        result.tag = .@"&=";
                        index += 1;
                        break;
                    },
                    else => {
                        result.tag = .@"&";
                        break;
                    },
                },
                .@"!" => switch (c) {
                    '=' => {
                        result.tag = .@"!=";
                        index += 1;
                        break;
                    },
                    else => {
                        result.tag = .@"!";
                        break;
                    },
                },
                .@"=" => switch (c) {
                    '=' => {
                        result.tag = .@"==";
                        index += 1;
                        break;
                    },
                    else => {
                        result.tag = .@"=";
                        break;
                    },
                },
                .@"<" => switch (c) {
                    '<' => state = .@"<<",
                    '=' => {
                        result.tag = .@"<=";
                        index += 1;
                        break;
                    },
                    else => {
                        result.tag = .@"<";
                        break;
                    },
                },
                .@"<<" => switch (c) {
                    '=' => {
                        result.tag = .@"<<=";
                        index += 1;
                        break;
                    },
                    else => {
                        result.tag = .@"<<";
                        break;
                    },
                },
                .@">" => switch (c) {
                    '>' => state = .@">>",
                    '=' => {
                        result.tag = .@">=";
                        index += 1;
                        break;
                    },
                    else => {
                        result.tag = .@">";
                        break;
                    },
                },
                .@">>" => switch (c) {
                    '=' => {
                        result.tag = .@">>=";
                        index += 1;
                        break;
                    },
                    else => {
                        result.tag = .@">>";
                        break;
                    },
                },
                .@"-" => switch (c) {
                    '-' => {
                        result.tag = .@"--";
                        index += 1;
                        break;
                    },
                    '=' => {
                        result.tag = .@"-=";
                        index += 1;
                        break;
                    },
                    '>' => {
                        result.tag = .@"->";
                        index += 1;
                        break;
                    },
                    '0'...'9' => {
                        // workaround for x-1 being tokenized as [x] [-1]
                        // TODO: maybe it's user fault? :^)
                        // duplicated at .@"+" too
                        if (index >= 2 and std.ascii.isAlphabetic(self.buffer[index - 2])) {
                            result.tag = .@"-";
                            break;
                        }
                        state = .{ .number = .{} };
                    },
                    else => {
                        result.tag = .@"-";
                        break;
                    },
                },
                .@"%" => switch (c) {
                    '=' => {
                        result.tag = .@"%=";
                        index += 1;
                        break;
                    },
                    else => {
                        result.tag = .@"%";
                        break;
                    },
                },
                .@"|" => switch (c) {
                    '|' => {
                        result.tag = .@"||";
                        index += 1;
                        break;
                    },
                    '=' => {
                        result.tag = .@"|=";
                        index += 1;
                        break;
                    },
                    else => {
                        result.tag = .@"|";
                        break;
                    },
                },
                .@"." => switch (c) {
                    '0'...'9' => state = .{ .number = .{} },
                    else => {
                        result.tag = .@".";
                        break;
                    },
                },
                .@"+" => switch (c) {
                    '+' => {
                        result.tag = .@"++";
                        index += 1;
                        break;
                    },
                    '=' => {
                        result.tag = .@"+=";
                        index += 1;
                        break;
                    },
                    '0'...'9' => {
                        if (index >= 2 and std.ascii.isAlphabetic(self.buffer[index - 2])) {
                            result.tag = .@"+";
                            break;
                        }
                        state = .{ .number = .{} };
                    },
                    else => {
                        result.tag = .@"+";
                        break;
                    },
                },
                .@"/" => switch (c) {
                    '/' => state = .line_comment,
                    '*' => state = .block_comment,
                    '=' => {
                        result.tag = .@"/=";
                        index += 1;
                        break;
                    },
                    else => {
                        result.tag = .@"/";
                        break;
                    },
                },
                .@"*" => switch (c) {
                    '=' => {
                        result.tag = .@"*=";
                        index += 1;
                        break;
                    },
                    else => {
                        result.tag = .@"*";
                        break;
                    },
                },
                .@"^" => switch (c) {
                    '=' => {
                        result.tag = .@"^=";
                        index += 1;
                        break;
                    },
                    else => {
                        result.tag = .@"^";
                        break;
                    },
                },
                .@"'" => switch (c) {
                    '\'' => {
                        result.tag = .string_literal;
                        index += 1;
                        break;
                    },
                    else => {},
                },
                .@"\"" => switch (c) {
                    '"' => {
                        result.tag = .string_literal;
                        index += 1;
                        break;
                    },
                    else => {},
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
};

fn testTokenize(buffer: [:0]const u8, comptime expected_token_tags: []const Token.Tag) !void {
    var tokenizer = Tokenizer.init(buffer);
    const allocator = std.testing.allocator;
    var tokens = std.MultiArrayList(Token){};
    defer tokens.deinit(allocator);
    while (true) {
        const token = tokenizer.next();
        try tokens.append(allocator, token);
        if (token.tag == .eof) break;
    }
    try std.testing.expectEqualSlices(Token.Tag, expected_token_tags ++ &[_]Token.Tag{Token.Tag.eof}, tokens.items(.tag));
    const last_token = tokens.pop();
    try std.testing.expectEqual(buffer.len, last_token.loc.end);
    try std.testing.expectEqual(buffer.len, last_token.loc.start);
}

test "identifiers" {
    try testTokenize("iden", &.{.ident});
    try testTokenize("iden0i", &.{.ident});
    try testTokenize("_", &.{._});
    try testTokenize("__", &.{.invalid});
    try testTokenize("_iden", &.{.ident});
}

test "numbers" {
    try testTokenize(
        \\10.0 10f 10u 10i 10
    , &.{
        .number,
        .number,
        .number,
        .number,
        .number,
    });
}

test "comments" {
    try testTokenize(
        \\// comment
        \\/*
        \\ block*comment
        \\ */iden
    , &.{.ident});
}

test "EOF comment" {
    try testTokenize(
        \\iden// asdf
    , &.{.ident});
}

test "function" {
    try testTokenize(
        \\// comment
        \\fn D_GGX(NH: f32, roughness : f32) -> f32 { return 1.0f; }
    , &.{
        .k_fn,
        .ident,
        .@"(",
        .ident,
        .@":",
        .ident,
        .@",",
        .ident,
        .@":",
        .ident,
        .@")",
        .@"->",
        .ident,
        .@"{",
        .k_return,
        .number,
        .@";",
        .@"}",
    });
}

test "import" {
    try testTokenize(
        \\// comment
        \\// import { Foo, Bart } from './foo.wgsl';
    , &.{
        .k_import,
        .@"{",
        .ident,
        .@",",
        .ident,
        .@"}",
        .k_from,
        .string_literal,
        .@";",
    });
}

test "diagnostic" {
    try testTokenize(
        \\@diagnostic(error, foo.bar)
    , &.{
        .@"@",
        .k_diagnostic,
        .@"(",
        .ident,
        .@",",
        .ident,
        .@".",
        .ident,
        .@")",
    });
}

test "template list" {
    try testTokenize(
        \\var a = array<u32,2>(0u, 1u)
    , &.{ .k_var, .ident, .@"=", .ident, .@"<", .ident, .@",", .number, .@">", .@"(", .number, .@",", .number, .@")" });
}
