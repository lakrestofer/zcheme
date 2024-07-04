const std = @import("std");
const isWhitespace = std.ascii.isWhitespace;
const test_lexer = @import("./tests/test.zig").test_lexer;
const test_prod = @import("./tests/test.zig").test_prod;
const test_prod_base = @import("./tests/test.zig").test_prod_base;

// struct Lexer {
input: []const u8,
pos: usize,
filter_atmosphere: bool = false,
//}

pub const LexerOptions = struct {
    filter_atmosphere: bool = false,
};

pub const TokenKind = enum {
    INVALID, // some invalid token
    IDENTIFIER,
    BOOLEAN,
    CHARACTER,
    STRING,
    NUMBER,
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
    COMMENT,
    WHITESPACE, // ' \n\r\t' and whatever char isWhitespace deems to be a whitespace
    INTERTOKEN_SPACE, // any space taken up by comments and whitespace
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

pub fn init(
    input: []const u8,
    options: LexerOptions,
) Self {
    return Self{
        .input = input,
        .pos = 0,
        // should intertoken_space be included in the output or not?
        .filter_atmosphere = options.filter_atmosphere,
    };
}

pub fn nextToken(self: *Self) ?Token {
    // consumes any whitespace or comments
    if (self.filter_atmosphere) _ = intertoken_space(self.input, &self.pos);

    if (eof(self.input, &self.pos)) return null;

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
    if (quasi_quote(self.input, &self.pos)) return Token.new(.QUASI_QUOTE, start, self.pos);
    if (quote(self.input, &self.pos)) return Token.new(.QUOTE, start, self.pos);
    if (unquote_splicing(self.input, &self.pos)) return Token.new(.UNQUOTE_SPLICING, start, self.pos);
    if (unquote(self.input, &self.pos)) return Token.new(.UNQUOTE, start, self.pos);
    if (syntax(self.input, &self.pos)) return Token.new(.SYNTAX, start, self.pos);
    if (quasi_syntax(self.input, &self.pos)) return Token.new(.QUASI_SYNTAX, start, self.pos);
    if (unsyntax_splicing(self.input, &self.pos)) return Token.new(.UNSYNTAX_SPLICING, start, self.pos);
    if (unsyntax(self.input, &self.pos)) return Token.new(.UNSYNTAX, start, self.pos);
    if (comment(self.input, &self.pos)) return Token.new(.COMMENT, start, self.pos);
    // try to parse number before parsing identifier such that (+i, -i) gets interpreted as numbers
    if (number(self.input, &self.pos)) return Token.new(.NUMBER, start, self.pos);
    if (identifier(self.input, &self.pos)) return Token.new(.IDENTIFIER, start, self.pos);
    if (boolean(self.input, &self.pos)) return Token.new(.BOOLEAN, start, self.pos);
    if (character(self.input, &self.pos)) return Token.new(.CHARACTER, start, self.pos);
    if (string(self.input, &self.pos)) return Token.new(.STRING, start, self.pos);

    // if we found not invalid pos we increment pos by one,
    // such that we still progress somehow
    self.pos += 1;

    return Token.new(.INVALID, start, self.pos);
}

// === production rules begin ===

fn eof(input: []const u8, pos: *usize) bool {
    return pos.* >= input.len;
}

fn whitespace(input: []const u8, pos: *usize) bool {
    if (!isWhitespace(input[pos.*])) return false;
    while (pos.* < input.len and isWhitespace(input[pos.*])) pos.* += 1;
    return true;
}

fn l_paren(input: []const u8, pos: *usize) bool {
    return match_char('(', input, pos);
}
fn r_paren(input: []const u8, pos: *usize) bool {
    return match_char(')', input, pos);
}
fn l_square_paren(input: []const u8, pos: *usize) bool {
    return match_char('[', input, pos);
}
fn r_square_paren(input: []const u8, pos: *usize) bool {
    return match_char(']', input, pos);
}
fn l_curly_paren(input: []const u8, pos: *usize) bool {
    return match_char('{', input, pos);
}
fn r_curly_paren(input: []const u8, pos: *usize) bool {
    return match_char('}', input, pos);
}
fn l_vec_paren(input: []const u8, pos: *usize) bool {
    return match_str("#(", input, pos);
}
fn l_byte_vec_paren(input: []const u8, pos: *usize) bool {
    return match_str("#vu8(", input, pos);
}
fn quote(input: []const u8, pos: *usize) bool {
    return match_char('\'', input, pos);
}
fn quasi_quote(input: []const u8, pos: *usize) bool {
    return match_char('`', input, pos);
}
fn unquote(input: []const u8, pos: *usize) bool {
    return match_char(',', input, pos);
}
fn unquote_splicing(input: []const u8, pos: *usize) bool {
    return match_str(",@", input, pos);
}
fn syntax(input: []const u8, pos: *usize) bool {
    return match_str("#'", input, pos);
}
fn quasi_syntax(input: []const u8, pos: *usize) bool {
    return match_str("#`", input, pos);
}
fn unsyntax(input: []const u8, pos: *usize) bool {
    return match_str("#,", input, pos);
}
fn unsyntax_splicing(input: []const u8, pos: *usize) bool {
    return match_str("#,@", input, pos);
}

fn dubble_quote(input: []const u8, pos: *usize) bool {
    return match_char('"', input, pos);
}
fn semicolon(input: []const u8, pos: *usize) bool {
    return match_char(';', input, pos);
}
fn hashtag(input: []const u8, pos: *usize) bool {
    return match_char('#', input, pos);
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

    if (!match_str("#|", input, &p)) return false; // p += 1

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

    if (!match_str("|#", input, &p)) return false;

    // having successfully parsed the comment
    // we update the cursor position of the caller
    pos.* = p;
    return true;
}

fn atmosphere(input: []const u8, pos: *usize) bool {
    return whitespace(input, pos) or comment(input, pos);
}

fn intertoken_space(input: []const u8, pos: *usize) bool {
    // since we run this at the start of every nextToken call
    if (input.len <= pos.*) return false;
    if (!atmosphere(input, pos)) return false;
    while (pos.* < input.len and atmosphere(input, pos)) {}
    return true;
}

fn identifier(input: []const u8, pos: *usize) bool {
    // either initial subsequent*
    var p = pos.*;

    if (initial(input, &p)) {
        while (p < input.len and subsequent(input, &p)) {}

        if (!delimiter_termination(input, p)) return false;

        pos.* = p;
        return true;
    }
    if (!peculiar_identifier(input, &p)) return false;
    if (!delimiter_termination(input, p)) return false;
    pos.* = p;
    // or peculiar identifier
    return true;
}

fn boolean(input: []const u8, pos: *usize) bool {
    return match_str("#t", input, pos) or
        match_str("#T", input, pos) or
        match_str("#f", input, pos) or
        match_str("#F", input, pos);
}

const CHARACTER_NAMES: [12][]const u8 = .{
    "nul", "alarm", "backspace", "tab", "linefeed", "newline", "vtab", "page", "return", "esc", "space", "delete",
};
fn character(input: []const u8, pos: *usize) bool {
    var p = pos.*;
    // p_diff = 0
    // #\
    if (!match_str("#\\", input, &p)) return false;
    // p_diff = 2

    // #\character_name
    // if (terminal_string("nul", input, &p)) ...
    // if (terminal_string("alarm", input, &p)) ...
    inline for (CHARACTER_NAMES) |name| {
        if (match_str(name, input, &p)) {
            pos.* = p;
            return true;
        }
    }
    // #\x
    p += 1;
    if (input[p - 1] == 'x') {
        if (input[p..].len > 0 and hex_scalar_value(input, &p)) {
            pos.* = p;
            return true;
        }
    }

    // the last case (#\{any_char}) becomes implicit
    // since p already points after 'any_char'
    // below

    if (!delimiter_termination(input, p)) return false;

    pos.* = p;
    return true;
}

fn string(input: []const u8, pos: *usize) bool {
    if (!match_str("\"", input, pos)) return false;

    while (pos.* < input.len and string_element(input, pos)) {}

    if (!match_str("\"", input, pos)) return false;

    return true;
}

fn intraline_whitespace(input: []const u8, pos: *usize) bool {
    return match_str("\t", input, pos) or match_str(" ", input, pos);
}

fn string_element(input: []const u8, pos: *usize) bool {
    if (input[pos.*] == '\"') return false;

    // check for some correct usages of '\'
    if ((inline_hex_escape(input, pos) or
        match_str("\\\t", input, pos) or // intraline_whitespace rule
        match_str("\\ ", input, pos) or // intraline_whitespace rule
        match_str("\\a", input, pos) or
        match_str("\\b", input, pos) or
        match_str("\\t", input, pos) or
        match_str("\\n", input, pos) or
        match_str("\\v", input, pos) or
        match_str("\\f", input, pos) or
        match_str("\\r", input, pos) or
        match_str("\\\"", input, pos) or
        match_str("\\\\", input, pos))) return true;

    if (input[pos.*] == '\\') return false;

    // NOTE the gramar specifies some other internal rule

    // the char is some other char (valid), increment the cursor
    pos.* += 1;

    return true;
}
// identifiers, characters, numbers and strings need to be terminated
// by some delimiter or eof
// does not advance pos (taken by copy and not reference)
fn delimiter_termination(input: []const u8, pos: usize) bool {
    var p = pos;
    return eof(input, &p) or
        delimiter(input, &p);
}

// NOTE: only checks for the delimiter
fn delimiter(input: []const u8, pos: *usize) bool {
    return l_paren(input, pos) or
        r_paren(input, pos) or
        l_square_paren(input, pos) or
        r_square_paren(input, pos) or
        dubble_quote(input, pos) or
        semicolon(input, pos) or
        hashtag(input, pos) or
        whitespace(input, pos);
}

fn initial(input: []const u8, pos: *usize) bool {
    return constituent(input, pos) or
        special_initial(input, pos) or
        inline_hex_escape(input, pos);
}

fn constituent(input: []const u8, pos: *usize) bool {
    return letter(input, pos);
}

fn inline_hex_escape(input: []const u8, pos: *usize) bool {
    var p = pos.*;
    if (!match_str("\\x", input, &p)) return false;
    if (!hex_scalar_value(input, &p)) return false;
    if (!match_str(";", input, &p)) return false;
    pos.* = p;
    return true;
}

fn hex_scalar_value(input: []const u8, pos: *usize) bool {
    // hex_digit+
    if (!hex_digit(input, pos)) return false;
    while (pos.* < input.len and hex_digit(input, pos)) {}
    return true;
}
fn hex_digit(input: []const u8, pos: *usize) bool {
    if (pos.* >= input.len) return false;
    if (digit10(input, pos)) return true;

    if ((65 <= input[pos.*] and input[pos.*] <= 70) or // A-F
        (97 <= input[pos.*] and input[pos.*] <= 102)) // a-f
    {
        pos.* += 1;
        return true;
    }
    return false;
}

fn letter(input: []const u8, pos: *usize) bool {
    if (!std.ascii.isAlphabetic(input[pos.*])) return false;
    pos.* += 1;
    return true;
}

const SPECIAL = "!$%&*/:<=>?^_~";
fn special_initial(input: []const u8, pos: *usize) bool {
    const input_c = input[pos.*];
    for (SPECIAL) |c| if (input_c == c) {
        pos.* += 1;
        return true;
    };
    return false;
}

fn subsequent(input: []const u8, pos: *usize) bool {
    return initial(input, pos) or
        digit10(input, pos) or
        special_subsequent(input, pos);
}

fn peculiar_identifier(input: []const u8, pos: *usize) bool {
    if (std.mem.startsWith(u8, input[pos.*..], "->")) {
        pos.* += 2;
        while (pos.* < input.len and subsequent(input, pos)) {}
        return true;
    }
    if (input[pos.*] == '+') {
        pos.* += 1;
        return true;
    }
    if (input[pos.*] == '-') {
        pos.* += 1;
        return true;
    }
    if (std.mem.startsWith(u8, input[pos.*..], "...")) {
        pos.* += 3;
        return true;
    }
    return false;
}

fn digit10(input: []const u8, pos: *usize) bool {
    if (pos.* >= input.len or !std.ascii.isDigit(input[pos.*])) return false;
    pos.* += 1;
    return true;
}
fn special_subsequent(input: []const u8, pos: *usize) bool {
    const res = input[pos.*] == '+' or
        input[pos.*] == '-' or
        input[pos.*] == '.' or
        input[pos.*] == '@';

    if (res) pos.* += 1;

    return res;
}

fn number(input: []const u8, pos: *usize) bool {
    return (num(2, input, pos) or num(8, input, pos) or num(10, input, pos) or num(16, input, pos)) and delimiter_termination(input, pos.*);
}

fn num(base: comptime_int, input: []const u8, pos: *usize) bool {
    var p = pos.*;

    if (!prefix(base, input, &p)) return false;
    if (!complex(base, input, &p)) return false;

    pos.* = p;
    return true;
}
fn complex(base: comptime_int, input: []const u8, pos: *usize) bool {
    var p = pos.*;
    // <real R> @ <real R>
    if (real(base, input, &p) and match_char('@', input, &p) and real(base, input, &p)) {
        pos.* = p;
        return true;
    }
    p = pos.*;
    // <real R> + <ureal R> i
    // <real R> - <ureal R> i
    // <real R> + <naninf> i
    // <real R> - <naninf> i
    if (real(base, input, &p) and match_any_char("-+", input, &p) and (ureal(base, input, &p) or naninf(input, &p)) and match_char('i', input, &p)) {
        pos.* = p;
        return true;
    }
    p = pos.*;
    // <real R> + i
    // <real R> - i
    if (real(base, input, &p) and match_any_char("-+", input, &p) and match_char('i', input, &p)) {
        pos.* = p;
        return true;
    }
    p = pos.*;
    // <real R>
    if (real(base, input, &p)) {
        pos.* = p;
        return true;
    }
    p = pos.*;
    // + <ureal R> i
    // - <ureal R> i
    // + <naninf> i
    // - <naninf> i
    if (match_any_char("-+", input, &p) and (ureal(base, input, &p) or naninf(input, &p)) and match_char('i', input, &p)) {
        pos.* = p;
        return true;
    }
    if (match_any_char("-+", input, &p) and match_char('i', input, &p)) {
        pos.* = p;
        return true;
    }

    return false;
}
fn real(base: comptime_int, input: []const u8, pos: *usize) bool {
    var p = pos.*;
    if (sign(input, &p) and ureal(base, input, &p)) {
        pos.* = p;
        return true;
    }
    p = pos.*;
    if (match_char('+', input, &p) or match_char('-', input, &p)) {
        if (naninf(input, &p)) {
            pos.* = p;
            return true;
        }
    }
    return false;
}
fn naninf(input: []const u8, pos: *usize) bool {
    return match_str("nan.0", input, pos) or match_str("inf.0", input, pos);
}
fn ureal(base: comptime_int, input: []const u8, pos: *usize) bool {
    var p = pos.*;

    // <uinteger R> / uinteger R>
    if (uinteger(base, input, &p) and
        match_char('/', input, &p) and
        uinteger(base, input, &p))
    {
        pos.* = p;
        return true;
    }
    p = pos.*;
    // <uinteger R>
    if (uinteger(base, input, &p)) {
        pos.* = p;
        return true;
    }
    p = pos.*;
    if (decimal(10, input, &p) and mantissa_width(input, &p)) {
        pos.* = p;
        return true;
    }
    return false;
}
fn decimal(base: comptime_int, input: []const u8, pos: *usize) bool {
    if (base != 10) @compileError("invalid base! decimal notation is only defined for base 10");

    var p = pos.*;
    // <digit 10>+ . <digit 10>* suffix
    if (uinteger(10, input, &p) and match_char('.', input, &p)) {
        _ = uinteger(10, input, &p); // optional
        if (suffix(input, &p)) {
            pos.* = p;
            return true;
        }
        return false;
    }
    p = pos.*;
    // <digit 10>+ <suffix>
    if (uinteger(10, input, &p) and suffix(input, &p)) {
        pos.* = p;
        return true;
    }
    p = pos.*;
    // . <digit 10>+ suffix
    if (match_char('.', input, &p) and uinteger(10, input, &p) and suffix(input, &p)) {
        pos.* = p;
        return true;
    }

    return false;
}
fn uinteger(base: comptime_int, input: []const u8, pos: *usize) bool {
    var p = pos.*;
    // match at least one
    if (!digit(base, input, &p)) return false;
    while (p < input.len and digit(base, input, &p)) {}
    pos.* = p;
    return true;
}
fn prefix(base: comptime_int, input: []const u8, pos: *usize) bool {
    var p = pos.*;
    if (radix(base, input, &p) and exactness(input, &p)) {
        pos.* = p;
        return true;
    }
    _ = exactness(input, pos); // always returns true
    _ = radix(base, input, pos); // optional
    return true;
}
fn suffix(input: []const u8, pos: *usize) bool {
    var p = pos.*;

    if (exponent_marker(input, &p) and sign(input, &p) and uinteger(10, input, &p)) {
        pos.* = p;
        return true;
    }
    // <empty>
    return true;
}
fn exponent_marker(input: []const u8, pos: *usize) bool {
    return match_any_char("eEsSfFdDlL", input, pos);
}
fn mantissa_width(input: []const u8, pos: *usize) bool {
    var p = pos.*;
    // | <uinteger 10>
    if (match_char('|', input, &p) and uinteger(10, input, &p)) {
        pos.* = p;
        return true;
    }
    // <empty>
    return true;
}
fn sign(input: []const u8, pos: *usize) bool {
    // optional + or -
    _ = match_any_char("+-", input, pos);
    return true;
}
fn exactness(input: []const u8, pos: *usize) bool {
    var p = pos.*;

    if (match_char('#', input, &p)) {
        if (match_any_char("iIeE", input, &p)) {
            pos.* = p;
            return true;
        }
        return false; // syntax error
    }

    return true;
}
fn radix(base: comptime_int, input: []const u8, pos: *usize) bool {
    var p = pos.*;
    const matched: bool = switch (base) {
        2 => match_char('#', input, &p) and match_any_char("bB", input, &p),
        8 => match_char('#', input, &p) and match_any_char("oO", input, &p),
        10 => (match_char('#', input, &p) and match_any_char("dD", input, &p)) or true,
        16 => match_char('#', input, &p) and match_any_char("xX", input, &p),
        else => @compileError("invalid base! must be one of 2,8,10 or 16"),
    };
    if (matched) pos.* = p;
    return matched;
}

fn digit(base: comptime_int, input: []const u8, pos: *usize) bool {
    switch (base) {
        2 => return match_any_char("01", input, pos),
        8 => return match_any_char("01234567", input, pos),
        10 => return digit10(input, pos),
        16 => return hex_digit(input, pos),
        else => @compileError("invalid base! must be one of 2,8,10 or 16"),
    }
}

// === production rules end===

// utils
inline fn match_str(comptime expected: []const u8, input: []const u8, pos: *usize) bool {
    if (pos.* >= input.len) return false;

    if (std.mem.startsWith(u8, input[pos.*..], expected)) {
        pos.* += expected.len;
        return true;
    }
    return false;
}
// match the provided char `char` only
inline fn match_char(comptime char: u8, input: []const u8, pos: *usize) bool {
    if (pos.* >= input.len) return false;

    if (pos.* < input.len and char == input[pos.*]) {
        pos.* += 1;
        return true;
    }
    return false;
}
// match any of the chars in the set `chars`
inline fn match_any_char(comptime chars: []const u8, input: []const u8, pos: *usize) bool {
    if (pos.* >= input.len) return false;

    const c = input[pos.*];
    for (chars) |char| {
        if (char == c) {
            pos.* += 1;
            return true;
        }
    }
    return false;
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

test intertoken_space {
    try test_prod(intertoken_space, " \n   \t  ; a comment \n   \r  #| some other comment |#  ", 53);
}

test identifier {
    // special initial
    try test_prod(identifier, "!", 1);
    try test_prod(identifier, "$", 1);
    try test_prod(identifier, "%", 1);
    try test_prod(identifier, "&", 1);
    try test_prod(identifier, "*", 1);
    try test_prod(identifier, "/", 1);
    try test_prod(identifier, ":", 1);
    try test_prod(identifier, "<", 1);
    try test_prod(identifier, "=", 1);
    try test_prod(identifier, ">", 1);
    try test_prod(identifier, "?", 1);
    try test_prod(identifier, "^", 1);
    try test_prod(identifier, "_", 1);
    try test_prod(identifier, "~", 1);

    // constituent subsequent* identifier
    try test_prod(identifier, "imAnIdentifier", 14);

    // using inline hex escape
    try test_prod(identifier, "\\x0123456789abcdefABCDEF;", 25); // TRIVIA: this value is just north of 2^80

    // some identifiers from the r6rs spec
    try test_prod(identifier, "lambda", 6);
    try test_prod(identifier, "q", 1);
    try test_prod(identifier, "soup", 4);
    try test_prod(identifier, "list->vector", 12);
    try test_prod(identifier, "+", 1);
    try test_prod(identifier, "V17a", 4);
    try test_prod(identifier, "<=", 2);
    try test_prod(identifier, "a34kTMNs", 8);
    try test_prod(identifier, "->-", 3);
    try test_prod(identifier, "the-word-recursion-has-many-meanings", 36);
}

test digit {
    // base 2
    {
        var pos: usize = 0;
        try std.testing.expect(digit(2, "0", &pos));
        try std.testing.expectEqual(1, pos);
        pos = 0;
        try std.testing.expect(digit(2, "1", &pos));
        try std.testing.expectEqual(1, pos);
        pos = 0;
        try std.testing.expect(!digit(2, "2", &pos));
        try std.testing.expectEqual(0, pos);
    }
    // base 8
    const digits = "0123456789aAbBcCdDeEfFNONSENSE"; // 8 should not be parsed in base 8
    {
        var i: usize = 0;
        while (i < 8) : (i += 1) {
            var pos: usize = 0;
            std.testing.expect(digit(8, digits[i..], &pos)) catch |e| {
                std.debug.print("test failed for input: {s}", .{digits[i..]});
                return e;
            };
            try std.testing.expectEqual(1, pos);
        }
        var pos: usize = 0;
        try std.testing.expect(!digit(8, digits[i..], &pos));
        try std.testing.expectEqual(0, pos);
    }
    // base 10
    {
        var i: usize = 0;
        while (i < 10) : (i += 1) {
            var pos: usize = 0;
            std.testing.expect(digit(10, digits[i..], &pos)) catch |e| {
                std.debug.print("test failed for input: {s}", .{digits[i..]});
                return e;
            };
            try std.testing.expectEqual(1, pos);
        }
        var pos: usize = 0;
        try std.testing.expect(!digit(10, digits[i..], &pos));
        try std.testing.expectEqual(0, pos);
    }
    // base 16
    {
        var i: usize = 0;
        while (i < 22) : (i += 1) {
            var pos: usize = 0;
            std.testing.expect(digit(16, digits[i..], &pos)) catch |e| {
                std.debug.print("test failed for input: {s}", .{digits[i..]});
                return e;
            };
            try std.testing.expectEqual(1, pos);
        }
        var pos: usize = 0;
        std.testing.expect(!digit(16, digits[i..], &pos)) catch |e| {
            std.debug.print("test failed (match was true) for input: {s}", .{digits[i..]});
            return e;
        };
        try std.testing.expectEqual(0, pos);
    }
}

test radix {
    // base 2
    {
        var pos: usize = 0;
        try std.testing.expect(radix(2, "#b", &pos));
        try std.testing.expectEqual(2, pos);
        pos = 0;
        try std.testing.expect(radix(2, "#B", &pos));
        try std.testing.expectEqual(2, pos);
        pos = 0;
        try std.testing.expect(!radix(2, "#Q", &pos));
        try std.testing.expectEqual(0, pos);
    }
    // base 8
    {
        var pos: usize = 0;
        try std.testing.expect(radix(8, "#o", &pos));
        try std.testing.expectEqual(2, pos);
        pos = 0;
        try std.testing.expect(radix(8, "#O", &pos));
        try std.testing.expectEqual(2, pos);
        pos = 0;
        try std.testing.expect(!radix(8, "#Q", &pos));
        try std.testing.expectEqual(0, pos);
    }
    // base 10
    {
        var pos: usize = 0;
        try std.testing.expect(radix(10, "#d", &pos));
        try std.testing.expectEqual(2, pos);
        pos = 0;
        try std.testing.expect(radix(10, "#D", &pos));
        try std.testing.expectEqual(2, pos);
        pos = 0;
        try std.testing.expect(radix(10, "whatever", &pos));
        try std.testing.expectEqual(0, pos);
    }
    // base 16
    {
        var pos: usize = 0;
        try std.testing.expect(radix(16, "#x", &pos));
        try std.testing.expectEqual(2, pos);
        pos = 0;
        try std.testing.expect(radix(16, "#X", &pos));
        try std.testing.expectEqual(2, pos);
        pos = 0;
        try std.testing.expect(!radix(16, "#Q", &pos));
        try std.testing.expectEqual(0, pos);
    }
}
test exactness {
    try test_prod(exactness, "#i", 2);
    try test_prod(exactness, "#I", 2);
    try test_prod(exactness, "#e", 2);
    try test_prod(exactness, "#E", 2);
    try std.testing.expectError(error.TestUnexpectedResult, test_prod(exactness, "#b", 0));
}
test sign {
    try test_prod(sign, "+", 1);
    try test_prod(sign, "-", 1);
    try test_prod(sign, "whatever", 0);
}
test mantissa_width {
    try test_prod(mantissa_width, "|123", 4);
    try test_prod(mantissa_width, "whatever", 0);
    try test_prod(mantissa_width, "|abc", 0);
    try test_prod(mantissa_width, "whatever", 0);
}
test exponent_marker {
    try test_prod(exponent_marker, "e", 1);
    try test_prod(exponent_marker, "E", 1);
    try test_prod(exponent_marker, "s", 1);
    try test_prod(exponent_marker, "S", 1);
    try test_prod(exponent_marker, "f", 1);
    try test_prod(exponent_marker, "F", 1);
    try test_prod(exponent_marker, "d", 1);
    try test_prod(exponent_marker, "D", 1);
    try test_prod(exponent_marker, "l", 1);
    try test_prod(exponent_marker, "L", 1);
}
test suffix {
    try test_prod(suffix, "whatever", 0);
    try test_prod(suffix, "e10", 3);
    try test_prod(suffix, "E+1", 3);
    try test_prod(suffix, "s+10", 4);
    try test_prod(suffix, "S1", 2);
    try test_prod(suffix, "f10", 3);
    try test_prod(suffix, "F1", 2);
    try test_prod(suffix, "d10", 3);
    try test_prod(suffix, "D1", 2);
    try test_prod(suffix, "l10", 3);
    try test_prod(suffix, "L1", 2);
}

test prefix {
    // base 2 prefixes
    try test_prod_base(prefix, 2, "#i", 2);
    try test_prod_base(prefix, 2, "#I", 2);
    try test_prod_base(prefix, 2, "#e", 2);
    try test_prod_base(prefix, 2, "#E", 2);
    try test_prod_base(prefix, 2, "#b", 2);
    try test_prod_base(prefix, 2, "#B", 2);
    try test_prod_base(prefix, 2, "#b#i", 4);
    try test_prod_base(prefix, 2, "#b#I", 4);
    try test_prod_base(prefix, 2, "#b#e", 4);
    try test_prod_base(prefix, 2, "#b#E", 4);
    try test_prod_base(prefix, 2, "#B#i", 4);
    try test_prod_base(prefix, 2, "#B#I", 4);
    try test_prod_base(prefix, 2, "#B#e", 4);
    try test_prod_base(prefix, 2, "#B#E", 4);
    // base 8 prefix
    try test_prod_base(prefix, 8, "#o", 2);
    try test_prod_base(prefix, 8, "#O", 2);
    try test_prod_base(prefix, 8, "#o#i", 4);
    try test_prod_base(prefix, 8, "#o#I", 4);
    try test_prod_base(prefix, 8, "#o#e", 4);
    try test_prod_base(prefix, 8, "#o#E", 4);
    try test_prod_base(prefix, 8, "#O#i", 4);
    try test_prod_base(prefix, 8, "#O#I", 4);
    try test_prod_base(prefix, 8, "#O#e", 4);
    try test_prod_base(prefix, 8, "#O#E", 4);
}

test decimal {
    try test_prod_base(decimal, 10, "123", 3);
    try test_prod_base(decimal, 10, "123e10", 6);
    try test_prod_base(decimal, 10, "123e-10", 7);
    try test_prod_base(decimal, 10, "123e+10", 7);
    try test_prod_base(decimal, 10, ".123d+10", 8);
    try test_prod_base(decimal, 10, "1123.123d+10", 12);
}
