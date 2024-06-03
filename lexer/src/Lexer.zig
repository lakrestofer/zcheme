const std = @import("std");
const isWhitespace = std.ascii.isWhitespace;
const test_lexer = @import("./tests/test.zig").test_lexer;
const test_prod = @import("./tests/test.zig").test_prod;

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
    if (quote(self.input, &self.pos)) return Token.new(.QUOTE, start, self.pos);
    if (quasi_quote(self.input, &self.pos)) return Token.new(.QUASI_QUOTE, start, self.pos);
    if (unquote(self.input, &self.pos)) return Token.new(.UNQUOTE, start, self.pos);
    if (unquote_splicing(self.input, &self.pos)) return Token.new(.UNQUOTE_SPLICING, start, self.pos);
    if (syntax(self.input, &self.pos)) return Token.new(.SYNTAX, start, self.pos);
    if (quasi_syntax(self.input, &self.pos)) return Token.new(.QUASI_SYNTAX, start, self.pos);
    if (unsyntax(self.input, &self.pos)) return Token.new(.UNSYNTAX, start, self.pos);
    if (unsyntax_splicing(self.input, &self.pos)) return Token.new(.UNSYNTAX_SPLICING, start, self.pos);
    if (comment(self.input, &self.pos)) return Token.new(.COMMENT, start, self.pos);
    if (identifier(self.input, &self.pos)) return Token.new(.IDENTIFIER, start, self.pos);
    if (boolean(self.input, &self.pos)) return Token.new(.BOOLEAN, start, self.pos);

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

fn dubble_quote(input: []const u8, pos: *usize) bool {
    return terminal_string("\"", input, pos);
}
fn semicolon(input: []const u8, pos: *usize) bool {
    return terminal_string(";", input, pos);
}
fn hashtag(input: []const u8, pos: *usize) bool {
    return terminal_string("#", input, pos);
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
    return terminal_string("#t", input, pos) or
        terminal_string("#T", input, pos) or
        terminal_string("#f", input, pos) or
        terminal_string("#F", input, pos);
}

const CHARACTER_NAMES: [12][]const u8 = .{
    "nul", "alarm", "backspace", "tab", "linefeed", "newline", "vtab", "page", "return", "esc", "space", "delete",
};

fn character(input: []const u8, pos: *usize) bool {
    var p = pos.*;
    // p_diff = 0
    // #\
    if (!terminal_string("#\\", input, &p)) return false;
    // p_diff = 2

    // #\character_name
    // if (terminal_string("nul", input, &p)) ...
    // if (terminal_string("alarm", input, &p)) ...
    inline for (CHARACTER_NAMES) |name| {
        if (terminal_string(name, input, &p)) {
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
    // must contain space enough for \x'.' where . is some hex scalar
    if (input[p..].len < 3 or !std.mem.eql(u8, input[p..(p + 2)], "\\x")) return false;
    p += 2; // "\x"
    if (!hex_scalar_value(input, &p)) return false;
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
    if (digit(input, pos)) return true;

    if ((25 <= input[pos.*] and input[pos.*] <= 70) or // A-F
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
        digit(input, pos) or
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

fn digit(input: []const u8, pos: *usize) bool {
    if (!std.ascii.isDigit(input[pos.*])) return false;
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
    try test_prod(identifier, "\\x0123456789abcdefABCDEF", 24); // TRIVIA: this value is just north of 2^80

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

test boolean {
    try test_prod(boolean, "#t", 2);
    try test_prod(boolean, "#T", 2);
    try test_prod(boolean, "#f", 2);
    try test_prod(boolean, "#F", 2);
}

test character {
    // match all named characters
    inline for (CHARACTER_NAMES) |name| {
        try test_prod(character, "#\\" ++ name, name.len + 2);
    }
    // match characters using hex notation
    try test_prod(character, "#\\x0123456789abcdefABCDEF", 25); // TRIVIA: utf8 got NOTHING against this bad boy
    // match single char characters
    try test_prod(character, "#\\x", 3);
    try test_prod(character, "#\\;", 3);
    try test_prod(character, "#\\,", 3);
    try test_prod(character, "#\\;", 3);
    try test_prod(character, "#\\,", 3);
    try test_prod(character, "#\\'", 3);
    try test_prod(character, "#\\a", 3);
}
