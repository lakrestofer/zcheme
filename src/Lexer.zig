const std = @import("std");
const isWhitespace = std.ascii.isWhitespace;

// struct Lexer {
input: []const u8,
p: usize = 0,
//}

pub const TokenKind = union(enum) {
    invalid, // some invalid token
    identifier: []const u8,
    boolean: bool,
    character: u8,
    string: []const u8,
    integer: i64,
    float: f64,
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

    pub inline fn new(kind: TokenKind, start: usize, end: usize) Token {
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
            .{ self.kind, self.match.start, self.match.end },
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

    if (eof(self.input, &self.p)) |_| return null;

    const start = self.p;
    if (l_byte_vec_paren(self.input, &self.p)) |t| return t;
    if (l_vec_paren(self.input, &self.p)) |t| return t;
    if (l_paren(self.input, &self.p)) |t| return t;
    if (r_paren(self.input, &self.p)) |t| return t;
    if (l_square_paren(self.input, &self.p)) |t| return t;
    if (r_square_paren(self.input, &self.p)) |t| return t;
    if (l_curly_paren(self.input, &self.p)) |t| return t;
    if (r_curly_paren(self.input, &self.p)) |t| return t;
    if (quasi_quote(self.input, &self.p)) |t| return t;
    if (quote(self.input, &self.p)) |t| return t;
    if (unquote_splicing(self.input, &self.p)) |t| return t;
    if (unquote(self.input, &self.p)) |t| return t;
    if (period(self.input, &self.p)) |t| return t;
    if (syntax(self.input, &self.p)) |t| return t;
    if (quasi_syntax(self.input, &self.p)) |t| return t;
    if (unsyntax_splicing(self.input, &self.p)) |t| return t;
    if (unsyntax(self.input, &self.p)) |t| return t;
    if (float(self.input, &self.p)) |t| return t;
    if (integer(self.input, &self.p)) |t| return t;
    if (identifier(self.input, &self.p)) |t| return t;
    if (boolean(self.input, &self.p)) |t| return t;
    if (character(self.input, &self.p)) |t| return t;
    if (string(self.input, &self.p)) |t| return t;

    // if we found no valid token we increment pos by one,
    // such that we still progress somehow
    self.p += 1;

    return Token.new(.invalid, start, self.p);
}

// === production rules begin ===

fn eof(in: []const u8, p: *usize) ?Token {
    if (p.* >= in.len) return Token.new(.invalid, p.*, p.*);
}

fn whitespace(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (start >= in.len || !isWhitespace(in[start])) return null;
    while (p.* < in.len and isWhitespace(in[p.*])) p.* += 1;
    return Token.new(.whitespace, start, p.*);
}

fn l_paren(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.char('(', in, p)) |_| return Token.new(.l_paren, start, p.*);
    return null;
}
fn r_paren(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.char(')', in, p)) return Token.new(.r_paren, start, p.*);
    return null;
}
fn l_square_paren(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.char('[', in, p)) |_| return Token.new(.l_square_paren, start, p.*);
    return null;
}
fn r_square_paren(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.char(']', in, p)) |_| return Token.new(.r_square_paren, start, p.*);
    return null;
}
fn l_curly_paren(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.char('{', in, p)) |_| return Token.new(.l_curly_paren, start, p.*);
    return null;
}
fn r_curly_paren(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.char('}', in, p)) |_| return Token.new(.r_curly_paren, start, p.*);
    return null;
}
fn l_vec_paren(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.str("#(", in, p)) |_| return Token.new(.l_vec_paren, start, p.*);
    return null;
}
fn l_byte_vec_paren(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.str("#vu8(", in, p)) |_| return Token.new(.l_byte_vec_paren, start, p.*);
    return null;
}
fn quote(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.char('\'', in, p)) |_| return Token.new(.quote, start, p.*);
    return null;
}
fn quasi_quote(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.char('`', in, p)) |_| return Token.new(.quasi_quote, start, p.*);
    return null;
}
fn unquote(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.char(',', in, p)) |_| return Token.new(.unquote, start, p.*);
    return null;
}
fn unquote_splicing(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.str(",@", in, p)) |_| return Token.new(.unquote_splicing, start, p.*);
    return null;
}
fn period(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.char('.', in, p)) |_| return Token.new(.period, start, p.*);
    return null;
}
fn syntax(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.str("#'", in, p)) |_| return Token.new(.syntax, start, p.*);
    return null;
}
fn quasi_syntax(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.str("#`", in, p)) |_| return Token.new(.quasi_syntax, start, p.*);
    return null;
}
fn unsyntax(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.str("#,", in, p)) |_| return Token.new(.unsyntax, start, p.*);
    return null;
}
fn unsyntax_splicing(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.str("#,@", in, p)) |_| return Token.new(.unsyntax_splicing, start, p.*);
    return null;
}

fn dubble_quote(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.char('"', in, p)) |_| return Token.new(.quote, start, p.*);
    return null;
}
fn semicolon(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.char(';', in, p)) |_| return Token.new(.invalid, start, p.*);
    return null;
}
fn hashtag(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.char('#', in, p)) |_| return Token.new(.invalid, start, p.*);
    return null;
}

fn line_ending(in: []const u8, p: *usize) ?Token {
    if (U.str("\r\n", in, p)) |m| return m;
    if (U.any_char("\r\n", in, p)) |m| return m;
    return null;
}

fn comment(in: []const u8, p: *usize) ?Token {
    if (single_line_comment(in, p)) |m| return m;
    if (nested_comment(in, p)) |m| return m;
    return null;
}

fn single_line_comment(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    // ;
    if (U.char(';', in, p)) |_| {} else return null;
    // anything except line_ending and eof
    while (p.* < in.len and
        in[p.*] != '\r' and
        in[p.*] != '\n') p.* += 1;
    if (eof(in, p)) return Token.new(.invalid, start, p.*);
    if (line_ending(in, p)) return Token.new(.invalid, start, p.*);
    return U.reset(p, start);
}
fn nested_comment(in: []const u8, p: *usize) ?Token {
    const start = p.*;

    if (U.str("#|", in, &start)) |_| {} else return null;

    // comment text
    // TODO write this in terms of functions that return Match
    while (start < in.len - 1 and
        !(std.mem.eql(u8, in[p..(p + 2)], "#|") or
        std.mem.eql(u8, in[p..(p + 2)], "|#")))
    {
        p.* += 1;
    }
    // we've found something that was either "#|",
    // "|#" or we reached end of in

    // // nested comment
    _ = nested_comment(in, &start);

    // // comment_text
    while (p.* < in.len - 1 and
        !(std.mem.eql(u8, in[p.*..(p.* + 2)], "#|") or
        std.mem.eql(u8, in[p.*..(p.* + 2)], "|#")))
    {
        p.* += 1;
    }

    if (!U.str("|#", in, p)) {
        return U.reset(p, start);
    }

    // having successfully parsed the comment
    // we update the cursor position of the caller
    return Token.new(.invalid, start, p.*);
}

fn atmosphere(in: []const u8, pos: *usize) ?Token {
    if (whitespace(in, pos)) |m| return m;
    if (comment(in, pos)) |m| return m;
    return null;
}

fn intertoken_space(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (atmosphere(in, p)) |_| {} else return null;
    while (atmosphere(in, p)) |_| {}
    return Token.new(.invalid, start, p.*);
}

fn identifier(in: []const u8, p: *usize) ?Token {
    // either initial subsequent*
    const start = p.*;

    if (initial(in, p)) |_| {
        while (subsequent(in, p)) |_| {}

        if (term(in, p.*)) |_| {} else {
            return U.reset(p, start);
        }
        return Token.new(.invalid, start, p.*);
    }
    _ = U.reset(p, start);
    if (U.all(&[_]Token{
        peculiar_identifier(in, p),
        term(in, p.*),
    })) |_| {
        return Token.new(.invalid, start, p.*);
    }
    _ = U.reset(p, start);
    return null;
}

fn boolean(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.char('#', in, p)) |_| {} else return null;
    if (U.any_char("tT", in, p)) return Token.new(.{ .boolean = true }, start, p.*);
    if (U.any_char("fF", in, p)) return Token.new(.{ .boolean = false }, start, p.*);
    p.* = start;
    _ = U.reset(p, start);
    return null;
}

const NameValuePair = std.meta.Tuple(&.{ []const u8, u8 });
const CHAR_NAME_VALUE_MAP = std.StaticStringMap(u8).initComptime(&[_]NameValuePair{
    .{ "nul", '\x00' },
    .{ "alarm", '\x07' },
    .{ "backspace", '\x08' },
    .{ "tab", '\x09' },
    .{ "linefeed", '\x0A' },
    .{ "newline", '\x0A' },
    .{ "vtab", '\x0B' },
    .{ "page", '\x0C' },
    .{ "return", '\x0D' },
    .{ "esc", '\x1B' },
    .{ "space", ' ' },
    .{ "delete", '\x7F' },
});
fn character(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    // #\
    if (U.str("#\\", in, p)) |_| {} else return null;

    // #\character_name
    inline for (CHAR_NAME_VALUE_MAP.keys()) |name| {
        if (U.str(name, in, p)) |_| {
            if (term(in, p.*)) {
                const c = CHAR_NAME_VALUE_MAP.get(name).?;
                return Token.new(.{ .character = c }, start, p.*);
            }
        }
    }
    // #\x
    if (U.char('x', in, &start)) |_| {
        if (hex_scalar_value(in, &start)) |scalar| {
            if (term(in, p.*)) |_| {
                // TODO convert scalar to char value
                const c = 'x';
                _ = scalar;
                return Token.new(.{ .character = c }, start, p.*);
            }
        }
    }

    p.* += 1;

    // the last case (#\{any_char}) becomes implicit
    // since p already points after 'any_char'
    // below
    if (!term(in, p.*)) {
        _ = U.reset(p, start);
        return null;
    }

    const c = in[p.* - 1];
    return Token.new(.{ .character = c }, start, p.*);
}

fn string(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.str("\"", in, p)) |_| {} else return null;

    while (string_element(in, p)) {}

    if (U.str("\"", in, p)) |_| {} else {
        return U.reset(p, start);
    }

    return Token.new(.{ .string = "what" }, start, p.*);
}

fn intraline_whitespace(in: []const u8, p: *usize) ?Token {
    if (U.str("\t", in, p)) |t| return t;
    if (U.str(" ", in, p)) |t| return t;
}

fn string_element(in: []const u8, p: *usize) bool {
    const start = p.*;
    if (U.char('\"', in, p)) |_| return null;

    // check for some correct usages of '\'
    if (inline_hex_escape(in, p)) |m| return m;
    if (U.str("\\\t", in, p)) |m| return m; // intraline_whitespace rule)
    if (U.str("\\ ", in, p)) |m| return m; // intraline_whitespace rule)
    if (U.str("\\a", in, p)) |m| return m;
    if (U.str("\\b", in, p)) |m| return m;
    if (U.str("\\t", in, p)) |m| return m;
    if (U.str("\\n", in, p)) |m| return m;
    if (U.str("\\v", in, p)) |m| return m;
    if (U.str("\\f", in, p)) |m| return m;
    if (U.str("\\r", in, p)) |m| return m;
    if (U.str("\\\"", in, p)) |m| return m;
    if (U.str("\\\\", in, p)) |m| return m;

    if (in[p.*] == '\\') return null;

    // NOTE the gramar specifies some other internal rule

    // the char is some other char (valid), increment the cursor
    p.* += 1;

    return Token.new(.invalid, start, p.*);
}
// identifiers, characters, numbers and strings need to be terminated
// by some delimiter or eof
// does not advance pos (taken by copy and not reference)
fn term(in: []const u8, pos: usize) ?Token {
    const start = pos;
    var p = pos;
    if (eof(in, &p)) |_| return Token.new(.invalid, start, start);
    if (delimiter(in, &p)) |_| return Token.new(.invalid, start, start);
    return null;
}

// NOTE: only checks for the delimiter
fn delimiter(in: []const u8, p: *usize) ?Token {
    if (l_paren(in, p)) |t| return t;
    if (r_paren(in, p)) |t| return t;
    if (l_square_paren(in, p)) |t| return t;
    if (r_square_paren(in, p)) |t| return t;
    if (dubble_quote(in, p)) |t| return t;
    if (semicolon(in, p)) |t| return t;
    if (hashtag(in, p)) |t| return t;
    if (whitespace(in, p)) |t| return t;
    return null;
}

fn initial(in: []const u8, p: *usize) ?Token {
    if (constituent(in, p)) |m| return m;
    if (special_initial(in, p)) |m| return m;
    if (inline_hex_escape(in, p)) |m| return m;
    return null;
}

fn constituent(in: []const u8, pos: *usize) ?Token {
    return letter(in, pos);
}

fn inline_hex_escape(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.str("\\x", in, p)) |_| {} else return null;
    if (hex_scalar_value(in, p)) |_| {} else return U.reset(p, start);
    if (U.str(";", in, p)) |_| {} else return U.reset(p, start);
    return Token.new(.invalid, start, p.*);
}

fn hex_scalar_value(in: []const u8, pos: *usize) ?Token {
    const start = pos.*;
    // hex_digit+
    if (hex_digit(in, pos)) |_| {} else return null;
    while (hex_digit(in, pos)) |_| {}
    return Token.new(.invalid, start, pos.*);
}
fn hex_digit(in: []const u8, pos: *usize) ?Token {
    if (U.any_char("0123456789abcdefABCDEF", in, pos)) |m| return m;
    return null;
}

fn letter(in: []const u8, pos: *usize) ?Token {
    const start = pos.*;
    if (pos.* >= in.len) return null;
    if (!std.ascii.isAlphabetic(in[pos.*])) return null;
    pos.* += 1;
    return Token.new(.invalid, start, pos.*);
}

const SPECIAL = "!$%&*/:<=>?^_~";
fn special_initial(in: []const u8, p: *usize) ?Token {
    if (U.any_char(SPECIAL, in, p)) |m| return m;
    return null;
}

fn subsequent(in: []const u8, p: *usize) ?Token {
    if (initial(in, p)) |m| return m;
    if (digit10(in, p)) |m| return m;
    if (special_subsequent(in, p)) |m| return m;
    return null;
}

fn peculiar_identifier(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (U.str("->", in, p)) |_| {
        while (subsequent(in, p)) |_| {}
        return Token.new(.invalid, start, p.*);
    }
    if (U.any_char("+-", in, p)) |m| return m;
    if (U.str("...", in, p)) |m| return m;
    return null;
}

fn digit10(in: []const u8, pos: *usize) ?Token {
    return U.any_char("0123456789", in, pos);
}
fn special_subsequent(in: []const u8, pos: *usize) ?Token {
    return U.any_char("+-.@", in, pos);
}
// floating point syntax from
fn float(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    _ = U.any_char("+-", in, p); // optional +-

    // '0x' hex_int '.' hex_int ([pP] [-+]? dec_int)? skip
    if (U.all(&[_]?Token{
        U.str("0x", in, p),
        hex_int(in, p),
        period(in, p),
        hex_int(in, p),
    })) |_| {
        const checkpoint = p.*;
        if (U.all(&[_]?Token{
            U.any_char("pP", in, p),
            U.opt(U.any_char("-+", in, p)),
            dec_int(in, p),
        })) |_| {} else {
            _ = U.reset(p, checkpoint);
        }
        if (term(in, p.*)) |_| {
            const value: f64 = std.fmt.parseFloat(f64, in[start..p.*]) catch @panic("could not parse float");
            return Token.new(.{ .float = value }, start, p.*);
        }
    }
    _ = U.reset(p, start);
    // dec_int '.' dec_int ([eE] [-+]? dec_int)? skip
    if (U.all(&[_]?Token{
        dec_int(in, p),
        period(in, p),
        dec_int(in, p),
    })) |_| {
        const checkpoint = p.*;
        if (U.all(&[_]?Token{
            U.any_char("eE", in, p),
            U.opt(U.any_char("-+", in, p)),
            dec_int(in, p),
        })) |_| {} else {
            _ = U.reset(p, checkpoint);
        }
        if (term(in, p.*)) |_| {
            const value: f64 = std.fmt.parseFloat(f64, in[start..p.*]) catch @panic("could not parse float");
            return Token.new(.{ .float = value }, start, p.*);
        }
    }
    _ = U.reset(p, start);
    // '0x' hex_int [pP] [-+]? dec_int skip
    if (U.all(&[_]?Token{
        U.str("0x", in, p),
        hex_int(in, p),
        U.any_char("pP", in, p),
        U.opt(U.any_char("-+", in, p)),
        dec_int(in, p),
        term(in, p.*),
    })) |_| {
        const value: f64 = std.fmt.parseFloat(f64, in[start..p.*]) catch @panic("could not parse float");
        return Token.new(.{ .float = value }, start, p.*);
    }
    _ = U.reset(p, start);
    // dec_int [eE] [-+]? dec_int skip
    if (U.all(&[_]?Token{
        dec_int(in, p),
        U.any_char("eE", in, p),
        U.opt(U.any_char("-+", in, p)),
        dec_int(in, p),
        term(in, p.*),
    })) |_| {
        const value: f64 = std.fmt.parseFloat(f64, in[start..p.*]) catch @panic("could not parse float");
        return Token.new(.{ .float = value }, start, p.*);
    }
    return null;
}

fn integer(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    _ = U.any_char("+-", in, p); // optional +-
    // "0b" bin_int skip
    if (U.all(&[_]?Token{ U.str("0b", in, p), bin_int(in, p), term(in, p.*) })) |_| {
        const value = std.fmt.parseInt(i64, in[start..p.*], 0) catch @panic("Could not parse int");
        return Token.new(.{ .integer = value }, start, p.*);
    }
    _ = U.reset(p, start);
    // "0o" oct_int skip
    if (U.all(&[_]?Token{
        U.str("0o", in, p),
        oct_int(in, p),
        term(in, p.*),
    })) |_| {
        const value = std.fmt.parseInt(i64, in[start..p.*], 0) catch @panic("Could not parse int");
        return Token.new(.{ .integer = value }, start, p.*);
    }
    _ = U.reset(p, start);
    // "0x" hex_int skip
    if (U.all(&[_]?Token{
        U.str("0x", in, p),
        hex_int(in, p),
        term(in, p.*),
    })) |_| {
        const value = std.fmt.parseInt(i64, in[start..p.*], 0) catch @panic("Could not parse int");
        return Token.new(.{ .integer = value }, start, p.*);
    }
    _ = U.reset(p, start);
    // dec_int   skip
    if (U.all(&[_]?Token{
        dec_int(in, p),
        term(in, p.*),
    })) |_| {
        const value = std.fmt.parseInt(i64, in[start..p.*], 0) catch @panic("Could not parse int");
        return Token.new(.{ .integer = value }, start, p.*);
    }
    return U.reset(p, start);
}
fn hex_int(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (dec(in, p)) |_| {
        while (_dec(in, p)) |_| {}
        return Token.new(.invalid, start, p.*);
    }
    return U.reset(p, start);
}
fn dec_int(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (dec(in, p)) |_| {
        while (_dec(in, p)) |_| {}
        return Token.new(.invalid, start, p.*);
    }
    return U.reset(p, start);
}
fn bin_int(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (bin(in, p)) |_| {
        while (_bin(in, p)) |_| {}
        return Token.new(.invalid, start, p.*);
    }
    return U.reset(p, start);
}
fn oct_int(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    if (oct(in, p)) |_| {
        while (_oct(in, p)) |_| {}
        return Token.new(.invalid, start, p.*);
    }
    return U.reset(p, start);
}
fn hex(in: []const u8, p: *usize) ?Token {
    return U.any_char("0123456789abcdefABCDEF", in, p);
}
fn _hex(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    _ = U.char('_', in, p);
    if (hex(in, p)) |_| return Token.new(.invalid, start, p.*);
    return U.reset(p, start);
}
fn dec(in: []const u8, p: *usize) ?Token {
    return U.any_char("0123456789", in, p);
}
fn _dec(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    _ = U.char('_', in, p);
    if (dec(in, p)) |_| return Token.new(.invalid, start, p.*);
    return U.reset(p, start);
}
fn bin(in: []const u8, p: *usize) ?Token {
    return U.any_char("01", in, p);
}
fn _bin(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    _ = U.char('_', in, p);
    if (bin(in, p)) |_| return Token.new(.invalid, start, p.*);
    return U.reset(p, start);
}
fn oct(in: []const u8, p: *usize) ?Token {
    return U.any_char("01234567", in, p);
}
fn _oct(in: []const u8, p: *usize) ?Token {
    const start = p.*;
    _ = U.char('_', in, p);
    if (oct(in, p)) |_| return Token.new(.invalid, start, p.*);
    return U.reset(p, start);
}
// === production rules end===

// utils
const U = struct {
    fn reset(c: *usize, p: usize) ?Token {
        c.* = p;
        return null;
    }

    fn opt(token: ?Token) Token {
        if (token) |t| return t;
        return Token.new(.invalid, 0, 0);
    }

    fn all(matches: []const ?Token) ?[]const ?Token {
        if (matches.len == 0) return null;
        for (matches) |m| if (m == null) return null;
        return matches;
    }
    fn any(matches: []const ?Token) ?Token {
        if (matches.len == 0) return null;
        for (matches) |m| if (m) |t| t;
        return null;
    }

    fn str(comptime expected: []const u8, in: []const u8, p: *usize) ?Token {
        if (p.* >= in.len) return null;

        const start = p.*;
        if (std.mem.startsWith(u8, in[p.*..], expected)) {
            p.* += expected.len;
            return Token.new(.invalid, start, p.*);
        }
        return null;
    }
    // match the provided char `char` only
    fn char(comptime c: u8, in: []const u8, p: *usize) ?Token {
        if (p.* >= in.len) return null;

        const start = p.*;
        if (p.* < in.len and c == in[p.*]) {
            p.* += 1;
            return Token.new(.invalid, start, p.*);
        }
        return null;
    }
    // match any of the chars in the set `chars`
    fn any_char(comptime chars: []const u8, in: []const u8, p: *usize) ?Token {
        if (p.* >= in.len) return null;

        const start = p.*;
        const c = in[start];
        for (chars) |c_| {
            if (c_ == c) {
                p.* += 1;
                return Token.new(.invalid, start, p.*);
            }
        }
        return null;
    }
};

/// test a single production rule that should match
/// it should match the input, and return it's length
pub fn test_prod(
    prod: *const fn (input: []const u8, pos: *usize) ?Token,
    input: []const u8,
    expected_end: usize,
) !void {
    var pos: usize = 0;
    try std.testing.expect(prod(input, &pos) != null);
    try std.testing.expectEqual(expected_end, pos);
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
