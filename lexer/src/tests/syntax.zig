const test_allocator = std.testing.allocator;
const Lexer = @import("../Lexer.zig");
const Token = Lexer.Token;
const std = @import("std");
const utils = @import("./test.zig");
const test_lexer = utils.test_lexer;
const test_token = utils.test_token;

test "parens" {
    try test_token("(", Token.new(.L_PAREN, 0, 1));
    try test_token("#(", Token.new(.L_VEC_PAREN, 0, 2));
    try test_token(")", Token.new(.R_PAREN, 0, 1));
    try test_token("[", Token.new(.L_SQUARE_PAREN, 0, 1));
    try test_token("]", Token.new(.R_SQUARE_PAREN, 0, 1));
    try test_token("#vu8(", Token.new(.L_BYTE_VEC_PAREN, 0, 5));
}

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
