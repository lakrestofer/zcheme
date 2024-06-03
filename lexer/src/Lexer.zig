const std = @import("std");
const isWhitespace = std.ascii.isWhitespace;
const test_lexer = @import("./testing.zig").test_lexer;
const test_prod = @import("./testing.zig").test_prod;

// struct Lexer {
input: []const u8,
pos: usize,
end_reached: bool = false,
//}

pub const TokenKind = enum {
    EOF, // \xOO
    INVALID, // some invalid token
    WHITESPACE, // ' \n\r\t' and whatever char isWhitespace deems to be a whitespace
    L_PAREN, // (
    R_PAREN, // )
    L_SQUARE_PAREN, // [
    R_SQUARE_PAREN, // ]
    // L_CURLY_PAREN, // { reserved for future use, .INVALID for now
    // R_CURLY_PAREN, // } reserved for future use, .INVALID for now
    L_VEC_PAREN, // #(
    L_BYTE_VEC_PAREN, // #vu8(
    QUOTE, // '
    QUASI_QUOTE, // `
    UNQUOTE, // ,
    UNQUOTE_SPLICING, // ,@
    SYNTAX, // #'
    QUASI_SYNTAX, // #`
    UNSYNTAX, // #'
    UNSYNTAX_SPLICING, // #'@
    COMMMENT,
    IDENTIFIER,
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

pub fn nextToken(self: *Self) ?Token {
    if (self.end_reached) return null;
    if (self.input.len <= self.pos) {
        self.end_reached = true;
        return Token.new(.EOF, self.pos, self.pos);
    }

    // std.debug.print("current pos: {}\n", .{self.pos});

    const start = self.pos;
    if (whitespace(self.input, &self.pos)) return Token.new(.WHITESPACE, start, self.pos);
    if (l_byte_vec_paren(self.input, &self.pos)) return Token.new(.L_BYTE_VEC_PAREN, start, self.pos);
    if (l_vec_paren(self.input, &self.pos)) return Token.new(.L_VEC_PAREN, start, self.pos);
    if (l_paren(self.input, &self.pos)) return Token.new(.L_PAREN, start, self.pos);
    if (r_paren(self.input, &self.pos)) return Token.new(.R_PAREN, start, self.pos);
    if (l_square_paren(self.input, &self.pos)) return Token.new(.L_SQUARE_PAREN, start, self.pos);
    if (r_square_paren(self.input, &self.pos)) return Token.new(.R_SQUARE_PAREN, start, self.pos);
    if (l_curly_paren(self.input, &self.pos)) return Token.new(.INVALID, start, self.pos);
    if (r_curly_paren(self.input, &self.pos)) return Token.new(.INVALID, start, self.pos);
    if (quote(self.input, &self.pos)) return Token.new(.QUOTE, start, self.pos);
    if (quasi_quote(self.input, &self.pos)) return Token.new(.QUASI_QUOTE, start, self.pos);
    if (unquote(self.input, &self.pos)) return Token.new(.UNQUOTE, start, self.pos);
    if (unquote_splicing(self.input, &self.pos)) return Token.new(.UNQUOTE_SPLICING, start, self.pos);
    if (syntax(self.input, &self.pos)) return Token.new(.SYNTAX, start, self.pos);
    if (quasi_syntax(self.input, &self.pos)) return Token.new(.QUASI_SYNTAX, start, self.pos);
    if (unsyntax(self.input, &self.pos)) return Token.new(.UNSYNTAX, start, self.pos);
    if (unsyntax_splicing(self.input, &self.pos)) return Token.new(.UNSYNTAX_SPLICING, start, self.pos);
    if (comment(self.input, &self.pos)) return Token.new(.COMMMENT, start, self.pos);

    // if we found not invalid pos we increment pos by one,
    // such that we still progress somehow
    self.pos += 1;

    return Token.new(.INVALID, start, self.pos);
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

fn line_ending(input: []const u8, pos: *usize) bool {
    const rest = input[pos.*..];
    if (rest.len >= 2 and std.mem.eql(u8, rest[0..2], "\r\n")) {
        pos.* += 2;
        return true;
    }
    if (input[pos.*] == '\r' or input[pos.*] == '\n') {
        pos.* += 1;
        return true;
    }
    return false;
}

fn comment(input: []const u8, pos: *usize) bool {
    return single_line_comment(input, pos) or
        nested_comment(input, pos);
    // nested_comment(input, pos) or
    // datum_comment(input, pos) or
    // extension_comment(input, pos);
}

fn single_line_comment(input: []const u8, pos: *usize) bool {
    var p = pos.*;
    // ;
    if (input[p] != ';') return false;
    p += 1;
    // anything except line_ending and eof
    while (p < input.len and input[p] != '\r' and input[p] != '\n')
        p += 1;
    if (p >= input.len or line_ending(input, &p)) {
        pos.* = p;
        return true;
    }
    return false;
}
fn nested_comment(input: []const u8, pos: *usize) bool {
    var p = pos.*;

    if (!terminal_string("#|", input, &p)) return false; // p += 1

    // comment text
    while (p < input.len - 1 and
        !(std.mem.eql(u8, input[p..(p + 2)], "#|") or
        std.mem.eql(u8, input[p..(p + 2)], "|#")))
    {
        p += 1;
    }
    // we've found something that was either "#|",
    // "|#" or we reached end of input

    // // nested comment
    _ = nested_comment(input, &p);

    // // comment_text
    while (p < input.len - 1 and
        !(std.mem.eql(u8, input[p..(p + 2)], "#|") or
        std.mem.eql(u8, input[p..(p + 2)], "|#")))
    {
        p += 1;
    }

    if (!terminal_string("|#", input, &p)) return false;

    // having successfully parsed the comment
    // we update the cursor position of the caller
    pos.* = p;
    return true;
}

TODO add testcase
fn atmosphere(input: []const u8, pos: *usize) bool {
    return whitespace(input, pos) or comment(input, pos);
}

TODO add testcase
fn intertoken_space(input: []const u8, pos: *usize) bool {
    if (!atmosphere(input, pos)) return false;
    while (atmosphere(input, pos)) {}
    return true;
}

TODO add testcase
fn identifier(input: []const u8, pos: *usize) bool {
    var p = pos.*;
    if (!initial(input, &p)) return false;

    pos.* = p;
    return true;
}

TODO add testcase
fn initial(input: []const u8, pos: *usize) bool {
    return constituent(input, pos) or
        special_initial(input, pos);
    // inline_hex_escape(input, pos);
}

TODO add testcase
fn constituent(input: []const u8, pos: *usize) bool {
    return letter(input, pos);
}

TODO add testcase
fn letter(input: []const u8, pos: *usize) bool {
    if (!std.ascii.isAlphabetic(input[pos.*])) return false;
    pos.* += 1;
}

TODO add testcase
fn special_initial(input: []const u8, pos: *usize) bool {
    const special = "!$%&*/:<=>?^_~";
    for (special) |c| {
        if (input[pos] == c) {
            pos.* += 1;
            return true;
        }
    }
    return false;
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
    try test_prod(l_square_paren, "[  ", 1);
}
test r_square_paren {
    try test_prod(r_square_paren, "]  ", 1);
}
test l_curly_paren {
    try test_prod(l_curly_paren, "{  ", 1);
}
test r_curly_paren {
    try test_prod(r_curly_paren, "}  ", 1);
}
test l_vec_paren {
    try test_prod(l_vec_paren, "#(   ", 2);
}
test l_byte_vec_paren {
    try test_prod(l_byte_vec_paren, "#vu8(  ", 5);
}

test quote {
    try test_prod(quote, "'  ", 1);
}
test quasi_quote {
    try test_prod(quasi_quote, "`  ", 1);
}
test unquote {
    try test_prod(unquote, ",  ", 1);
}
test unquote_splicing {
    try test_prod(unquote_splicing, ",@  ", 2);
}
test syntax {
    try test_prod(syntax, "#'  ", 2);
}
test quasi_syntax {
    try test_prod(quasi_syntax, "#`  ", 2);
}
test unsyntax {
    try test_prod(unsyntax, "#,  ", 2);
}
test unsyntax_splicing {
    try test_prod(unsyntax_splicing, "#,@   ", 3);
}
test line_ending {
    try test_prod(line_ending, "\r\n", 2);
    try test_prod(line_ending, "\r", 1);
    try test_prod(line_ending, "\n", 1);
}

test single_line_comment {
    try test_prod(single_line_comment, ";\r\n", 3);
    try test_prod(single_line_comment, ";\n", 2);
    try test_prod(single_line_comment, ";\n", 2);
    try test_prod(single_line_comment, ";", 1);
}

test nested_comment {
    try test_prod(nested_comment, "#||#", 4);
    try test_prod(nested_comment, "#| this is a comment |#", 23);
    try test_prod(nested_comment, "#| this is a comment with nested comment #| i am nested |# |#", 61);
    try test_prod(nested_comment, "#| before #| inner |# after |#", 30);
}

test "muliline_nested_comment" {
    const input =
        \\#|
        \\  I am before the nested comment
        \\  #|
        \\    I am nested
        \\  |#
        \\  I am after the nested comment
        \\|#
    ;
    try test_prod(nested_comment, input, 96);
}
