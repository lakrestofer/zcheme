const test_allocator = std.testing.allocator;
const Lexer = @import("../Lexer.zig");
const Token = Lexer.Token;
const std = @import("std");
const utils = @import("./test.zig");
const test_lexer = utils.test_lexer;
const test_token = utils.test_token;

test "identifiers" {
    try test_token("lambda", Token.new(.IDENTIFIER, 0, 6));
    try test_token("q", Token.new(.IDENTIFIER, 0, 1));
    try test_token("soup", Token.new(.IDENTIFIER, 0, 4));
    try test_token("list->vector", Token.new(.IDENTIFIER, 0, 12));
    try test_token("+", Token.new(.IDENTIFIER, 0, 1));
    try test_token("V17a", Token.new(.IDENTIFIER, 0, 4));
    try test_token("<=", Token.new(.IDENTIFIER, 0, 2));
    try test_token("a34kTMNs", Token.new(.IDENTIFIER, 0, 8));
    try test_token("->-", Token.new(.IDENTIFIER, 0, 3));
    try test_token("the-word-recursion-has-many-meanings", Token.new(.IDENTIFIER, 0, 36));
}

test "whitespace" {
    try test_token("   \n\r\t", Token.new(.WHITESPACE, 0, 6));
}

test "boolean" {
    try test_token("#t", Token.new(.BOOLEAN, 0, 2));
    try test_token("#T", Token.new(.BOOLEAN, 0, 2));
    try test_token("#f", Token.new(.BOOLEAN, 0, 2));
    try test_token("#F", Token.new(.BOOLEAN, 0, 2));
}
test "character" {
    // match all named characters
    try test_token("#\\nul", Token.new(.CHARACTER, 0, 5));
    try test_token("#\\alarm", Token.new(.CHARACTER, 0, 7));
    try test_token("#\\backspace", Token.new(.CHARACTER, 0, 11));
    try test_token("#\\tab", Token.new(.CHARACTER, 0, 5));
    try test_token("#\\linefeed", Token.new(.CHARACTER, 0, 10));
    try test_token("#\\newline", Token.new(.CHARACTER, 0, 9));
    try test_token("#\\vtab", Token.new(.CHARACTER, 0, 6));
    try test_token("#\\page", Token.new(.CHARACTER, 0, 6));
    try test_token("#\\return", Token.new(.CHARACTER, 0, 8));
    try test_token("#\\esc", Token.new(.CHARACTER, 0, 5));
    try test_token("#\\space", Token.new(.CHARACTER, 0, 7));
    try test_token("#\\delete", Token.new(.CHARACTER, 0, 8));
    // match characters using hex notation
    try test_token("#\\x0123456789abcdefABCDEF", Token.new(.CHARACTER, 0, 25)); // TRIVIA: utf8 got NOTHING against this bad boy
    // match single char characters
    try test_token("#\\x", Token.new(.CHARACTER, 0, 3));
    try test_token("#\\);", Token.new(.CHARACTER, 0, 3));
    try test_token("#\\,", Token.new(.CHARACTER, 0, 3));
    try test_token("#\\);", Token.new(.CHARACTER, 0, 3));
    try test_token("#\\,", Token.new(.CHARACTER, 0, 3));
    try test_token("#\\'", Token.new(.CHARACTER, 0, 3));
    try test_token("#\\a", Token.new(.CHARACTER, 0, 3));
}
test "string" {
    try test_token("\"this is a string\"", Token.new(.STRING, 0, 18));
    try test_token("\"another one\"", Token.new(.STRING, 0, 13));
    try test_token("\"\"", Token.new(.STRING, 0, 2));
    try test_token("\" \\t \\n \\v \\\" \\\\ \"", Token.new(.STRING, 0, 18));
    try test_token("\"\\ \\\t\"", Token.new(.STRING, 0, 6));
}

test "number" {
    try test_token("#e28.000", Token.new(.NUMBER, 0, 8));
    try test_token("#x1c", Token.new(.NUMBER, 0, 4));
    try test_token("3.1415926535898F0", Token.new(.NUMBER, 0, 17));
    // TODO fill this out with more number tests
}

test "parens" {
    try test_token("(", Token.new(.L_PAREN, 0, 1));
    try test_token("#(", Token.new(.L_VEC_PAREN, 0, 2));
    try test_token(")", Token.new(.R_PAREN, 0, 1));
    try test_token("[", Token.new(.L_SQUARE_PAREN, 0, 1));
    try test_token("]", Token.new(.R_SQUARE_PAREN, 0, 1));
    try test_token("#vu8(", Token.new(.L_BYTE_VEC_PAREN, 0, 5));
}
test "quotes" {
    try test_token("'", Token.new(.QUOTE, 0, 1));
    try test_token("`", Token.new(.QUASI_QUOTE, 0, 1));
    try test_token(",", Token.new(.UNQUOTE, 0, 1));
    try test_token(",@", Token.new(.UNQUOTE_SPLICING, 0, 2));
}

test "syntax" {
    try test_token("#'", Token.new(.SYNTAX, 0, 2));
    try test_token("#`", Token.new(.QUASI_SYNTAX, 0, 2));
    try test_token("#,", Token.new(.UNSYNTAX, 0, 2));
    try test_token("#,@", Token.new(.UNSYNTAX_SPLICING, 0, 3));
}

test "comment" {
    try test_token("; this is a long comment", Token.new(.COMMENT, 0, 24));
    const nested_comment =
        \\#|
        \\  this is a nested comment
        \\  this is a nested comment
        \\  this is a nested comment
        \\  #|
        \\    this is a nested comment
        \\    this is a nested comment
        \\    this is a nested comment
        \\  |#
        \\  this is a nested comment
        \\  this is a nested comment
        \\  this is a nested comment
        \\|#
    ;
    try test_token(nested_comment, Token.new(.COMMENT, 0, 264));
}

test "define" {
    try test_lexer("(define x 3)", &([_]Token{
        Token.new(.L_PAREN, 0, 1), // (
        Token.new(.IDENTIFIER, 1, 7), // heron
        Token.new(.IDENTIFIER, 8, 9), // x
        Token.new(.NUMBER, 10, 11), // 3
        Token.new(.R_PAREN, 11, 12), // )
    }), .{ .filter_atmosphere = true });
}

// test "heron" {
//     const input =
//         \\(define (heron a b c)
//         \\  (let ((s (/ (+ a b c) 2)))
//         \\     (sqrt (* s (- s a) (- s b) (- s c)))))
//     ;
//     try test_lexer(input, &([_]Token{
//         Token.new(.L_PAREN, 0, 35), // (
//         Token.new(.IDENTIFIER, 35, 36), // define
//         Token.new(.L_PAREN, 0, 35), // (
//         Token.new(.IDENTIFIER, 36, 37), // heron
//         Token.new(.IDENTIFIER, 37, 38), // a
//         Token.new(.IDENTIFIER, 38, 39), // b
//         Token.new(.IDENTIFIER, 38, 39), // c
//         Token.new(.R_PAREN, 0, 35), // )
//         Token.new(.L_PAREN, 0, 35), // (
//         Token.new(.IDENTIFIER, 38, 39), // let
//         Token.new(.L_PAREN, 0, 35), // (
//         Token.new(.L_PAREN, 0, 35), // (
//         Token.new(.IDENTIFIER, 38, 39), // s
//         Token.new(.L_PAREN, 0, 35), // (
//         Token.new(.IDENTIFIER, 38, 39), // /
//         Token.new(.L_PAREN, 0, 35), // (
//         Token.new(.IDENTIFIER, 38, 39), // +
//         Token.new(.IDENTIFIER, 37, 38), // a
//         Token.new(.IDENTIFIER, 38, 39), // b
//         Token.new(.IDENTIFIER, 38, 39), // c
//         Token.new(.R_PAREN, 0, 35), // )
//         Token.new(.NUMBER, 0, 35), // 2
//         Token.new(.R_PAREN, 0, 35), // )
//         Token.new(.R_PAREN, 0, 35), // )
//         Token.new(.R_PAREN, 0, 35), // )
//         Token.new(.L_PAREN, 0, 35), // (
//         Token.new(.IDENTIFIER, 38, 39), // sqrt
//         Token.new(.L_PAREN, 0, 35), // (
//         Token.new(.IDENTIFIER, 38, 39), // *
//         Token.new(.IDENTIFIER, 38, 39), // s
//         Token.new(.L_PAREN, 0, 35), // (
//         Token.new(.IDENTIFIER, 38, 39), // -
//         Token.new(.IDENTIFIER, 38, 39), // s
//         Token.new(.IDENTIFIER, 38, 39), // a
//         Token.new(.R_PAREN, 0, 35), // )
//         Token.new(.L_PAREN, 0, 35), // (
//         Token.new(.IDENTIFIER, 38, 39), // -
//         Token.new(.IDENTIFIER, 38, 39), // s
//         Token.new(.IDENTIFIER, 38, 39), // b
//         Token.new(.R_PAREN, 0, 35), // )
//         Token.new(.L_PAREN, 0, 35), // (
//         Token.new(.IDENTIFIER, 38, 39), // -
//         Token.new(.IDENTIFIER, 38, 39), // s
//         Token.new(.IDENTIFIER, 38, 39), // c
//         Token.new(.R_PAREN, 0, 35), // )
//         Token.new(.R_PAREN, 0, 35), // )
//         Token.new(.R_PAREN, 0, 35), // )
//         Token.new(.R_PAREN, 0, 35), // )
//         Token.new(.R_PAREN, 0, 35), // )
//     }), .{});
// }
