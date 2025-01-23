const std = @import("std");
const isWhitespace = std.ascii.isWhitespace;
const test_prod = @import("./tests/lexer.zig").test_prod;

// struct Lexer {
input: []const u8,
p: usize = 0,
//}

pub const TokenKind = enum {
    invalid, // some invalid token
    identifier,
    boolean,
    character,
    string,
    integer,
    float,
    l_paren, // (
    r_paren, // )
    l_square_paren, // [
    r_square_paren, // ]
    // L_CURLY_PAREN, // { reserved for future use, .INVALID for now
    // R_CURLY_PAREN, // } reserved for future use, .INVALID for now
    l_vec_paren, // #(
    l_byte_vec_paren, // #vu8(
    quote, // '
    quasi_quote, // `
    unquote, // ,
    unquote_splicing, // ,@
    period, // .
    syntax, // #'
    quasi_syntax, // #`
    unsyntax, // #'
    unsyntax_splicing, // #'@
    comment,
    whitespace, // ' \n\r\t' and whatever char isWhitespace deems to be a whitespace
    intertoken_space, // any space taken up by comments and whitespace
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

    pub fn format(
        self: Token,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print(
            "Token {{ .kind = {}, .start = {}, .end = {} }}",
            .{ self.kind, self.start, self.end },
        );
    }
};

const Self = @This();

pub fn init(
    input: []const u8,
) Self {
    return Self{ .input = input };
}

pub fn nextToken(self: *Self) ?Token {
    // consumes any whitespace or comments
    _ = intertoken_space(self.input, &self.p);

    if (eof(self.input, &self.p)) return null;

    const start = self.p;
    if (l_byte_vec_paren(self.input, &self.p)) return Token.new(.l_byte_vec_paren, start, self.p);
    if (l_vec_paren(self.input, &self.p)) return Token.new(.l_vec_paren, start, self.p);
    if (l_paren(self.input, &self.p)) return Token.new(.l_paren, start, self.p);
    if (r_paren(self.input, &self.p)) return Token.new(.r_paren, start, self.p);
    if (l_square_paren(self.input, &self.p)) return Token.new(.l_square_paren, start, self.p);
    if (r_square_paren(self.input, &self.p)) return Token.new(.r_square_paren, start, self.p);
    if (l_curly_paren(self.input, &self.p)) return Token.new(.invalid, start, self.p);
    if (r_curly_paren(self.input, &self.p)) return Token.new(.invalid, start, self.p);
    if (quasi_quote(self.input, &self.p)) return Token.new(.quasi_quote, start, self.p);
    if (quote(self.input, &self.p)) return Token.new(.quote, start, self.p);
    if (unquote_splicing(self.input, &self.p)) return Token.new(.unquote_splicing, start, self.p);
    if (unquote(self.input, &self.p)) return Token.new(.unquote, start, self.p);
    if (period(self.input, &self.p)) return Token.new(.period, start, self.p);
    if (syntax(self.input, &self.p)) return Token.new(.syntax, start, self.p);
    if (quasi_syntax(self.input, &self.p)) return Token.new(.quasi_syntax, start, self.p);
    if (unsyntax_splicing(self.input, &self.p)) return Token.new(.unsyntax_splicing, start, self.p);
    if (unsyntax(self.input, &self.p)) return Token.new(.unsyntax, start, self.p);
    if (float(self.input, &self.p)) return Token.new(.float, start, self.p);
    if (integer(self.input, &self.p)) return Token.new(.integer, start, self.p);
    if (identifier(self.input, &self.p)) return Token.new(.identifier, start, self.p);
    if (boolean(self.input, &self.p)) return Token.new(.boolean, start, self.p);
    if (character(self.input, &self.p)) return Token.new(.character, start, self.p);
    if (string(self.input, &self.p)) return Token.new(.string, start, self.p);

    // if we found not invalid pos we increment pos by one,
    // such that we still progress somehow
    self.p += 1;

    return Token.new(.invalid, start, self.p);
}

// === production rules begin ===

fn eof(in: []const u8, p: *usize) bool {
    return p.* >= in.len;
}

fn whitespace(in: []const u8, p: *usize) bool {
    if (!isWhitespace(in[p.*])) return false;
    while (p.* < in.len and isWhitespace(in[p.*])) p.* += 1;
    return true;
}

fn l_paren(in: []const u8, p: *usize) bool {
    return U.char('(', in, p);
}
fn r_paren(in: []const u8, p: *usize) bool {
    return U.char(')', in, p);
}
fn l_square_paren(in: []const u8, p: *usize) bool {
    return U.char('[', in, p);
}
fn r_square_paren(in: []const u8, p: *usize) bool {
    return U.char(']', in, p);
}
fn l_curly_paren(in: []const u8, p: *usize) bool {
    return U.char('{', in, p);
}
fn r_curly_paren(in: []const u8, p: *usize) bool {
    return U.char('}', in, p);
}
fn l_vec_paren(in: []const u8, p: *usize) bool {
    return U.str("#(", in, p);
}
fn l_byte_vec_paren(in: []const u8, p: *usize) bool {
    return U.str("#vu8(", in, p);
}
fn quote(in: []const u8, p: *usize) bool {
    return U.char('\'', in, p);
}
fn quasi_quote(in: []const u8, p: *usize) bool {
    return U.char('`', in, p);
}
fn unquote(in: []const u8, p: *usize) bool {
    return U.char(',', in, p);
}
fn unquote_splicing(in: []const u8, p: *usize) bool {
    return U.str(",@", in, p);
}
fn period(in: []const u8, p: *usize) bool {
    return U.char('.', in, p);
}
fn syntax(in: []const u8, p: *usize) bool {
    return U.str("#'", in, p);
}
fn quasi_syntax(in: []const u8, p: *usize) bool {
    return U.str("#`", in, p);
}
fn unsyntax(in: []const u8, p: *usize) bool {
    return U.str("#,", in, p);
}
fn unsyntax_splicing(in: []const u8, p: *usize) bool {
    return U.str("#,@", in, p);
}

fn dubble_quote(in: []const u8, p: *usize) bool {
    return U.char('"', in, p);
}
fn semicolon(in: []const u8, p: *usize) bool {
    return U.char(';', in, p);
}
fn hashtag(in: []const u8, p: *usize) bool {
    return U.char('#', in, p);
}

fn line_ending(in: []const u8, p: *usize) bool {
    if (U.str("\r\n", in, p) or
        U.any_char("\r\n", in, p)) return true;
    return false;
}

fn comment(in: []const u8, p: *usize) bool {
    return single_line_comment(in, p) or
        nested_comment(in, p);
    // nested_comment(in, pos) or
    // datum_comment(in, pos) or
    // extension_comment(in, pos);
}

fn single_line_comment(in: []const u8, p_: *usize) bool {
    var p = p_.*;
    // ;
    if (in[p] != ';') return false;
    p += 1;
    // anything except line_ending and eof
    while (p < in.len and in[p] != '\r' and in[p] != '\n')
        p += 1;
    if (p >= in.len or line_ending(in, &p)) {
        p_.* = p;
        return true;
    }
    return false;
}
fn nested_comment(in: []const u8, p_: *usize) bool {
    var p = p_.*;

    if (!U.str("#|", in, &p)) return false; // p += 1

    // comment text
    while (p < in.len - 1 and
        !(std.mem.eql(u8, in[p..(p + 2)], "#|") or
        std.mem.eql(u8, in[p..(p + 2)], "|#")))
    {
        p += 1;
    }
    // we've found something that was either "#|",
    // "|#" or we reached end of in

    // // nested comment
    _ = nested_comment(in, &p);

    // // comment_text
    while (p < in.len - 1 and
        !(std.mem.eql(u8, in[p..(p + 2)], "#|") or
        std.mem.eql(u8, in[p..(p + 2)], "|#")))
    {
        p += 1;
    }

    if (!U.str("|#", in, &p)) return false;

    // having successfully parsed the comment
    // we update the cursor position of the caller
    p_.* = p;
    return true;
}

fn atmosphere(in: []const u8, pos: *usize) bool {
    return whitespace(in, pos) or comment(in, pos);
}

fn intertoken_space(in: []const u8, pos: *usize) bool {
    // since we run this at the start of every nextToken call
    if (in.len <= pos.*) return false;
    if (!atmosphere(in, pos)) return false;
    while (pos.* < in.len and atmosphere(in, pos)) {}
    return true;
}

fn identifier(in: []const u8, pos: *usize) bool {
    // either initial subsequent*
    var p = pos.*;

    if (initial(in, &p)) {
        while (p < in.len and subsequent(in, &p)) {}

        if (!term(in, p)) return false;

        pos.* = p;
        return true;
    }
    if (!peculiar_identifier(in, &p)) return false;
    if (!term(in, p)) return false;
    pos.* = p;
    // or peculiar identifier
    return true;
}

fn boolean(in: []const u8, pos: *usize) bool {
    return U.str("#t", in, pos) or
        U.str("#T", in, pos) or
        U.str("#f", in, pos) or
        U.str("#F", in, pos);
}

const CHARACTER_NAMES: [12][]const u8 = .{
    "nul", "alarm", "backspace", "tab", "linefeed", "newline", "vtab", "page", "return", "esc", "space", "delete",
};
fn character(in: []const u8, pos: *usize) bool {
    var p = pos.*;
    // p_diff = 0
    // #\
    if (!U.str("#\\", in, &p)) return false;
    // p_diff = 2

    // #\character_name
    // if (terminal_string("nul", in, &p)) ...
    // if (terminal_string("alarm", in, &p)) ...
    inline for (CHARACTER_NAMES) |name| {
        if (U.str(name, in, &p)) {
            pos.* = p;
            return true;
        }
    }
    // #\x
    p += 1;
    if (in[p - 1] == 'x') {
        if (in[p..].len > 0 and hex_scalar_value(in, &p)) {
            pos.* = p;
            return true;
        }
    }

    // the last case (#\{any_char}) becomes implicit
    // since p already points after 'any_char'
    // below

    if (!term(in, p)) return false;

    pos.* = p;
    return true;
}

fn string(in: []const u8, pos: *usize) bool {
    if (!U.str("\"", in, pos)) return false;

    while (pos.* < in.len and string_element(in, pos)) {}

    if (!U.str("\"", in, pos)) return false;

    return true;
}

fn intraline_whitespace(in: []const u8, pos: *usize) bool {
    return U.str("\t", in, pos) or U.str(" ", in, pos);
}

fn string_element(in: []const u8, pos: *usize) bool {
    if (in[pos.*] == '\"') return false;

    // check for some correct usages of '\'
    if ((inline_hex_escape(in, pos) or
        U.str("\\\t", in, pos) or // intraline_whitespace rule
        U.str("\\ ", in, pos) or // intraline_whitespace rule
        U.str("\\a", in, pos) or
        U.str("\\b", in, pos) or
        U.str("\\t", in, pos) or
        U.str("\\n", in, pos) or
        U.str("\\v", in, pos) or
        U.str("\\f", in, pos) or
        U.str("\\r", in, pos) or
        U.str("\\\"", in, pos) or
        U.str("\\\\", in, pos))) return true;

    if (in[pos.*] == '\\') return false;

    // NOTE the gramar specifies some other internal rule

    // the char is some other char (valid), increment the cursor
    pos.* += 1;

    return true;
}
// identifiers, characters, numbers and strings need to be terminated
// by some delimiter or eof
// does not advance pos (taken by copy and not reference)
fn term(in: []const u8, pos: usize) bool {
    var p = pos;
    return eof(in, &p) or
        delimiter(in, &p);
}

// NOTE: only checks for the delimiter
fn delimiter(in: []const u8, pos: *usize) bool {
    return l_paren(in, pos) or
        r_paren(in, pos) or
        l_square_paren(in, pos) or
        r_square_paren(in, pos) or
        dubble_quote(in, pos) or
        semicolon(in, pos) or
        hashtag(in, pos) or
        whitespace(in, pos);
}

fn initial(in: []const u8, pos: *usize) bool {
    return constituent(in, pos) or
        special_initial(in, pos) or
        inline_hex_escape(in, pos);
}

fn constituent(in: []const u8, pos: *usize) bool {
    return letter(in, pos);
}

fn inline_hex_escape(in: []const u8, pos: *usize) bool {
    var p = pos.*;
    if (!U.str("\\x", in, &p)) return false;
    if (!hex_scalar_value(in, &p)) return false;
    if (!U.str(";", in, &p)) return false;
    pos.* = p;
    return true;
}

fn hex_scalar_value(in: []const u8, pos: *usize) bool {
    // hex_digit+
    if (!hex_digit(in, pos)) return false;
    while (pos.* < in.len and hex_digit(in, pos)) {}
    return true;
}
fn hex_digit(in: []const u8, pos: *usize) bool {
    if (pos.* >= in.len) return false;
    if (digit10(in, pos)) return true;

    if ((65 <= in[pos.*] and in[pos.*] <= 70) or // A-F
        (97 <= in[pos.*] and in[pos.*] <= 102)) // a-f
    {
        pos.* += 1;
        return true;
    }
    return false;
}

fn letter(in: []const u8, pos: *usize) bool {
    if (!std.ascii.isAlphabetic(in[pos.*])) return false;
    pos.* += 1;
    return true;
}

const SPECIAL = "!$%&*/:<=>?^_~";
fn special_initial(in: []const u8, pos: *usize) bool {
    const in_c = in[pos.*];
    for (SPECIAL) |c| if (in_c == c) {
        pos.* += 1;
        return true;
    };
    return false;
}

fn subsequent(in: []const u8, pos: *usize) bool {
    return initial(in, pos) or
        digit10(in, pos) or
        special_subsequent(in, pos);
}

fn peculiar_identifier(in: []const u8, pos: *usize) bool {
    if (std.mem.startsWith(u8, in[pos.*..], "->")) {
        pos.* += 2;
        while (pos.* < in.len and subsequent(in, pos)) {}
        return true;
    }
    if (in[pos.*] == '+') {
        pos.* += 1;
        return true;
    }
    if (in[pos.*] == '-') {
        pos.* += 1;
        return true;
    }
    if (std.mem.startsWith(u8, in[pos.*..], "...")) {
        pos.* += 3;
        return true;
    }
    return false;
}

fn digit10(in: []const u8, pos: *usize) bool {
    return U.any_char("0123456789", in, pos);
}
fn special_subsequent(in: []const u8, pos: *usize) bool {
    return U.any_char("+-.@", in, pos);
}

// floating point syntax from
fn float(in: []const u8, pos: *usize) bool {
    var p = pos.*;
    _ = U.any_char("+-", in, &p); // optional +-
    // "0x" hex_int "." hex_int ([pP] [-+]? dec_int)? skip
    if (U.str("0x", in, &p) and
        hex_int(in, &p) and
        U.char('.', in, &p) and
        hex_int(in, &p))
    {
        var p2 = p;
        if (U.any_char("pP", in, &p2) and
            (U.any_char("-+", in, &p2) or true) and
            dec_int(in, &p2))
        {
            p = p2;
        }
        if (term(in, p)) return U.matched(pos, p);
    }
    p = pos.*;
    // dec_int "." dec_int ([eE] [-+]? dec_int)? skip
    if (dec_int(in, &p) and
        U.char('.', in, &p) and
        dec_int(in, &p))
    {
        var p2 = p;
        if (U.any_char("eE", in, &p2) and
            (U.any_char("-+", in, &p2) or true) and
            dec_int(in, &p2))
        {
            p = p2;
        }
        if (term(in, p)) return U.matched(pos, p);
    }
    p = pos.*;
    // "0x" hex_int [pP] [-+]? dec_int skip
    if (U.str("0x", in, &p) and
        hex_int(in, &p) and
        U.any_char("pP", in, &p) and
        (U.any_char("-+", in, &p) or true) and
        dec_int(in, &p) and
        term(in, p))
        return U.matched(pos, p);
    p = pos.*;
    //  dec_int [eE] [-+]? dec_int skip
    if (dec_int(in, &p) and
        U.any_char("eE", in, &p) and
        (U.any_char("-+", in, &p) or true) and
        dec_int(in, &p) and
        term(in, p))
        return U.matched(pos, p);
    return false;
}

fn integer(in: []const u8, pos: *usize) bool {
    var p = pos.*;
    _ = U.any_char("+-", in, &p); // optional +-
    // "0b" bin_int skip
    if (U.str("0b", in, &p) and bin_int(in, &p) and term(in, p)) return U.matched(pos, p);
    p = pos.*;
    // "0o" oct_int skip
    if (U.str("0o", in, &p) and oct_int(in, &p) and term(in, p)) return U.matched(pos, p);
    p = pos.*;
    // "0x" hex_int skip
    if (U.str("0x", in, &p) and hex_int(in, &p) and term(in, p)) return U.matched(pos, p);
    p = pos.*;
    // dec_int   skip
    if (dec_int(in, &p) and term(in, p)) return U.matched(pos, p);
    return false;
}
fn hex_int(in: []const u8, pos: *usize) bool {
    var p = pos.*;
    if (hex(in, &p)) {
        while (_hex(in, &p)) {}
        return U.matched(pos, p);
    }
    return false;
}
fn dec_int(in: []const u8, pos: *usize) bool {
    var p = pos.*;
    if (dec(in, &p)) {
        while (_dec(in, &p)) {}
        return U.matched(pos, p);
    }
    return false;
}
fn bin_int(in: []const u8, pos: *usize) bool {
    var p = pos.*;
    if (bin(in, &p)) {
        while (_bin(in, &p)) {}
        return U.matched(pos, p);
    }
    return false;
}
fn oct_int(in: []const u8, pos: *usize) bool {
    var p = pos.*;
    if (oct(in, &p)) {
        while (_oct(in, &p)) {}
        return U.matched(pos, p);
    }
    return false;
}
fn hex(in: []const u8, pos: *usize) bool {
    return U.any_char("0123456789abcdefABCDEF", in, pos);
}
fn _hex(in: []const u8, pos: *usize) bool {
    var p = pos.*;
    _ = U.char('_', in, &p);
    if (hex(in, &p)) return U.matched(pos, p);
    return false;
}
fn dec(in: []const u8, pos: *usize) bool {
    return U.any_char("0123456789", in, pos);
}
fn _dec(in: []const u8, pos: *usize) bool {
    var p = pos.*;
    _ = U.char('_', in, &p);
    if (dec(in, &p)) return U.matched(pos, p);
    return false;
}
fn bin(in: []const u8, pos: *usize) bool {
    return U.any_char("01", in, pos);
}
fn _bin(in: []const u8, pos: *usize) bool {
    var p = pos.*;
    _ = U.char('_', in, &p);
    if (bin(in, &p)) return U.matched(pos, p);
    return false;
}
fn oct(in: []const u8, pos: *usize) bool {
    return U.any_char("01234567", in, pos);
}
fn _oct(in: []const u8, pos: *usize) bool {
    var p = pos.*;
    _ = U.char('_', in, &p);
    if (oct(in, &p)) return U.matched(pos, p);
    return false;
}
// === production rules end===

// utils
const U = struct {
    inline fn str(comptime expected: []const u8, in: []const u8, pos: *usize) bool {
        if (pos.* >= in.len) return false;

        if (std.mem.startsWith(u8, in[pos.*..], expected)) {
            pos.* += expected.len;
            return true;
        }
        return false;
    }
    // match the provided char `char` only
    inline fn char(comptime c: u8, in: []const u8, pos: *usize) bool {
        if (pos.* >= in.len) return false;

        if (pos.* < in.len and c == in[pos.*]) {
            pos.* += 1;
            return true;
        }
        return false;
    }
    // match any of the chars in the set `chars`
    inline fn any_char(comptime chars: []const u8, in: []const u8, pos: *usize) bool {
        if (pos.* >= in.len) return false;

        const c = in[pos.*];
        for (chars) |c_| {
            if (c_ == c) {
                pos.* += 1;
                return true;
            }
        }
        return false;
    }
    inline fn matched(p: *usize, new_p: usize) bool {
        p.* = new_p;
        return true;
    }
};

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
    const in =
        \\#|
        \\  I am before the nested comment
        \\  #|
        \\    I am nested
        \\  |#
        \\  I am after the nested comment
        \\|#
    ;
    try test_prod(nested_comment, in, 96);
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

test bin_int {
    try test_prod(integer, "0b11110000", 10); // binary_int
}

test integer {
    try test_prod(integer, "98222", 5); // decimal_int
    try test_prod(integer, "0xff", 4); // hex_int
    try test_prod(integer, "0xFF", 4); // another_hex_int
    try test_prod(integer, "0o755", 5); // octal_int
    try test_prod(integer, "0b11110000", 10); // binary_int
    try test_prod(integer, "1_000_000_000", 13); // one_billion
    try test_prod(integer, "0b1_1111_1111", 13); // binary_mask
    try test_prod(integer, "0o7_5_5", 7); // permissions
    try test_prod(integer, "0xFF80_0000_0000_0000", 21); // big_address
}

test float {
    try test_prod(float, "123.0E+77", 9); // floating_point
    try test_prod(float, "123.0", 5); // another_float
    try test_prod(float, "123.0e+77", 9); // yet_another

    try test_prod(float, "0x103.70p-5", 11); // hex_floating_point
    try test_prod(float, "0x103.70", 8); // another_hex_float
    try test_prod(float, "0x103.70P-5", 11); // yet_another_hex_float

    // underscores may be placed between two digits as a visual separator
    try test_prod(float, "299_792_458.000_000", 19); // lightspeed
    try test_prod(float, "0.000_000_001", 13); // nanosecond
    try test_prod(float, "0x1234_5678.9ABC_CDEFp-10", 25); // more_hex
}
