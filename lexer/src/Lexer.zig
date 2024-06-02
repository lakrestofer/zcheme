const std = @import("std");
const isWhitespace = std.ascii.isWhitespace;
const test_lexer = @import("./testing.zig").test_lexer;
const test_prod = @import("./testing.zig").test_prod;

// struct Lexer {
input: []const u8,
pos: usize,
//}

pub const TokenKind = enum {
    EOF,
    INVALID,
    WHITESPACE,
    L_PAREN,
    R_PAREN,
};

pub const Token = struct {
    kind: TokenKind,
    start: usize,
    end: usize,

    pub fn new(kind: TokenKind, start: usize, end: usize) Token {
        return Token{
            .kind = kind,
            .start = start,
            .end = end,
        };
    }
};

const Self = @This();

pub fn init(input: []const u8) Self {
    return Self{
        .input = input,
        .pos = 0,
    };
}

pub fn nextToken(self: *Self) Token {
    const start = self.pos;
    var end = self.pos;
    var kind: TokenKind = .INVALID;

    if (self.input.len <= end) return Token.new(.EOF, start, end);

    if (whitespace(self.input, &end)) kind = .WHITESPACE;
    if (l_paren(self.input, &end)) kind = .L_PAREN;
    if (r_paren(self.input, &end)) kind = .R_PAREN;

    self.pos = end;

    return Token.new(kind, start, end);
}

// === production rules begin ===
fn whitespace(input: []const u8, pos: *usize) bool {
    if (!isWhitespace(input[pos.*])) return false;
    while (pos.* < input.len and isWhitespace(input[pos.*])) pos.* += 1;
    return true;
}

fn l_paren(input: []const u8, pos: *usize) bool {
    return terminal_string("(", input, pos);
}
fn r_paren(input: []const u8, pos: *usize) bool {
    return terminal_string(")", input, pos);
}
fn l_square_paren(input: []const u8, pos: *usize) bool {
    return terminal_string("[", input, pos);
}
fn r_square_paren(input: []const u8, pos: *usize) bool {
    return terminal_string("]", input, pos);
}
fn l_curly_paren(input: []const u8, pos: *usize) bool {
    return terminal_string("{", input, pos);
}
fn r_curly_paren(input: []const u8, pos: *usize) bool {
    return terminal_string("}", input, pos);
}
fn l_vec_paren(input: []const u8, pos: *usize) bool {
    return terminal_string("#(", input, pos);
}
fn l_byte_vec_paren(input: []const u8, pos: *usize) bool {
    return terminal_string("#vu8(", input, pos);
}
fn quote(input: []const u8, pos: *usize) bool {
    return terminal_string("'", input, pos);
}
fn quasi_quote(input: []const u8, pos: *usize) bool {
    return terminal_string("`", input, pos);
}
fn unquote(input: []const u8, pos: *usize) bool {
    return terminal_string(",", input, pos);
}
fn unquote_splicing(input: []const u8, pos: *usize) bool {
    return terminal_string(",@", input, pos);
}
fn syntax(input: []const u8, pos: *usize) bool {
    return terminal_string("#'", input, pos);
}
fn quasi_syntax(input: []const u8, pos: *usize) bool {
    return terminal_string("#`", input, pos);
}
fn unsyntax(input: []const u8, pos: *usize) bool {
    return terminal_string("#,", input, pos);
}
fn unsyntax_splicing(input: []const u8, pos: *usize) bool {
    return terminal_string("#,@", input, pos);
}
// === production rules end===

// utils
inline fn terminal_string(comptime expected: []const u8, input: []const u8, pos: *usize) bool {
    if (std.mem.startsWith(u8, input[pos.*..], expected)) {
        pos.* += expected.len;
        return true;
    }
    return false;
}

test whitespace {
    try test_prod(whitespace, "   \n\r\t", 6);
}
test l_paren {
    try test_prod(l_paren, "(", 1);
}
test r_paren {
    try test_prod(r_paren, ")", 1);
}
test l_square_paren {
    try test_prod(l_square_paren, "[", 1);
}
test r_square_paren {
    try test_prod(r_square_paren, "]", 1);
}
test l_curly_paren {
    try test_prod(l_curly_paren, "{", 1);
}
test r_curly_paren {
    try test_prod(r_curly_paren, "}", 1);
}
test l_vec_paren {
    try test_prod(l_vec_paren, "#(", 2);
}
test l_byte_vec_paren {
    try test_prod(l_byte_vec_paren, "#vu8(", 5);
}

test quote {
    try test_prod(quote, "'", 1);
}
test quasi_quote {
    try test_prod(quasi_quote, "`", 1);
}
test unquote {
    try test_prod(unquote, ",", 1);
}
test unquote_splicing {
    try test_prod(unquote_splicing, ",@", 2);
}
test syntax {
    try test_prod(syntax, "#'", 2);
}
test quasi_syntax {
    try test_prod(quasi_syntax, "#`", 2);
}
test unsyntax {
    try test_prod(unsyntax, "#,", 2);
}
test unsyntax_splicing {
    try test_prod(unsyntax_splicing, "#,@", 3);
}
