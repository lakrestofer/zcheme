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

// here we define some example syntactic constructs
test "define" {
    try test_lexer("(define x 3)", &([_]Token{
        Token.new(.L_PAREN, 0, 1), // (
        Token.new(.IDENTIFIER, 1, 7), // define
        Token.new(.IDENTIFIER, 8, 9), // x
        Token.new(.NUMBER, 10, 11), // 3
        Token.new(.R_PAREN, 11, 12), // )
    }), .{ .filter_atmosphere = true });
}

test "heron" {
    const input =
        \\(define (heron a b c)
        \\  (let ((s (/ (+ a b c) 2)))
        \\     (sqrt (* s (- s a) (- s b) (- s c)))))
    ;
    try test_lexer(input, &([_]Token{
        Token.new(.L_PAREN, 0, 1), // (
        Token.new(.IDENTIFIER, 1, 7), // define
        Token.new(.L_PAREN, 8, 9), // (
        Token.new(.IDENTIFIER, 9, 14), // heron
        Token.new(.IDENTIFIER, 15, 16), // a
        Token.new(.IDENTIFIER, 17, 18), // b
        Token.new(.IDENTIFIER, 19, 20), // c
        Token.new(.R_PAREN, 20, 21), // )
        Token.new(.L_PAREN, 24, 25), // (
        Token.new(.IDENTIFIER, 25, 28), // let
        Token.new(.L_PAREN, 29, 30), // (
        Token.new(.L_PAREN, 30, 31), // (
        Token.new(.IDENTIFIER, 31, 32), // s
        Token.new(.L_PAREN, 33, 34), // (
        Token.new(.IDENTIFIER, 34, 35), // /
        Token.new(.L_PAREN, 36, 37), // (
        Token.new(.IDENTIFIER, 37, 38), // +
        Token.new(.IDENTIFIER, 39, 40), // a
        Token.new(.IDENTIFIER, 41, 42), // b
        Token.new(.IDENTIFIER, 43, 44), // c
        Token.new(.R_PAREN, 44, 45), // )
        Token.new(.NUMBER, 46, 47), // 3
        Token.new(.R_PAREN, 47, 48), // )
        Token.new(.R_PAREN, 48, 49), // )
        Token.new(.R_PAREN, 49, 50), // )
        Token.new(.L_PAREN, 56, 57), // (
        Token.new(.IDENTIFIER, 57, 61), // sqrt
        Token.new(.L_PAREN, 62, 63), // (
        Token.new(.IDENTIFIER, 63, 64), // *
        Token.new(.IDENTIFIER, 65, 66), // s
        Token.new(.L_PAREN, 67, 68), // (
        Token.new(.IDENTIFIER, 68, 69), // -
        Token.new(.IDENTIFIER, 70, 71), // s
        Token.new(.IDENTIFIER, 72, 73), // a
        Token.new(.R_PAREN, 73, 74), // )
        Token.new(.L_PAREN, 75, 76), // (
        Token.new(.IDENTIFIER, 76, 77), // -
        Token.new(.IDENTIFIER, 78, 79), // s
        Token.new(.IDENTIFIER, 80, 81), // b
        Token.new(.R_PAREN, 81, 82), // )
        Token.new(.L_PAREN, 83, 84), // (
        Token.new(.IDENTIFIER, 84, 85), // -
        Token.new(.IDENTIFIER, 86, 87), // s
        Token.new(.IDENTIFIER, 88, 89), // c
        Token.new(.R_PAREN, 89, 90), // )
        Token.new(.R_PAREN, 90, 91), // )
        Token.new(.R_PAREN, 91, 92), // )
        Token.new(.R_PAREN, 92, 93), // )
        Token.new(.R_PAREN, 93, 94), // )
    }), .{ .filter_atmosphere = true });
}

test "macro-example" {
    const input =
        \\ ;; (for-times (counter iterations) (state initial-value) . body)
        \\ ;;
        \\ ;; Execute body a number of times equal to iterations. counter and
        \\ ;; state are bound locally to the current iteration and the value
        \\ ;; returned by the previous iteration respectively.  Counter starts
        \\ ;; at zero and increments to 1 less than iterations.  State starts
        \\ ;; at initial-value and afterwards is the value returned by body
        \\ (define-macro for-times
        \\       (lambda (iterate state . body)
        \\       (let ((iterator (first iterate))
        \\               (iterations (second iterate))
        \\               (name (first state))
        \\               (value (second state)))
        \\       `(let loop ((,iterator 0)
        \\                       (,name ,value))
        \\               (if (< ,iterator ,iterations)
        \\               (loop (+ ,iterator 1) (begin ,@body))
        \\               ,name)))))
    ;
    try test_lexer(input, &([_]Token{
        Token.new(.L_PAREN, 409, 410), // (
        Token.new(.IDENTIFIER, 410, 422), // define-macro
        Token.new(.IDENTIFIER, 423, 432), // for-times
        Token.new(.L_PAREN, 440, 441), // (
        Token.new(.IDENTIFIER, 441, 447), // lambda
        Token.new(.L_PAREN, 448, 449), // (
        Token.new(.IDENTIFIER, 449, 456), // iterate
        Token.new(.IDENTIFIER, 457, 462), // state
        Token.new(.PERIOD, 463, 464), // .
        Token.new(.IDENTIFIER, 465, 469), // body
        Token.new(.R_PAREN, 469, 470), // )
        Token.new(.L_PAREN, 478, 479), // (
        Token.new(.IDENTIFIER, 479, 482), // let
        Token.new(.L_PAREN, 483, 484), // (
        Token.new(.L_PAREN, 484, 485), // (
        Token.new(.IDENTIFIER, 485, 493), // iterator
        Token.new(.L_PAREN, 494, 495), // (
        Token.new(.IDENTIFIER, 495, 500), // first
        Token.new(.IDENTIFIER, 501, 508), // iterate
        Token.new(.R_PAREN, 508, 509), // )
        Token.new(.R_PAREN, 509, 510), // )
        Token.new(.L_PAREN, 526, 527), // (
        Token.new(.IDENTIFIER, 527, 537), // iterations
        Token.new(.L_PAREN, 538, 539), // (
        Token.new(.IDENTIFIER, 539, 545), // second
        Token.new(.IDENTIFIER, 546, 553), // iterate
        Token.new(.R_PAREN, 553, 554), // )
        Token.new(.R_PAREN, 554, 555), // )
        Token.new(.L_PAREN, 571, 572), // (
        Token.new(.IDENTIFIER, 572, 576), // name
        Token.new(.L_PAREN, 577, 578), // (
        Token.new(.IDENTIFIER, 578, 583), // first
        Token.new(.IDENTIFIER, 584, 589), // state
        Token.new(.R_PAREN, 589, 590), // )
        Token.new(.R_PAREN, 590, 591), // )
        Token.new(.L_PAREN, 607, 608), // (
        Token.new(.IDENTIFIER, 608, 613), // value
        Token.new(.L_PAREN, 614, 615), // (
        Token.new(.IDENTIFIER, 615, 621), // second
        Token.new(.IDENTIFIER, 622, 627), // state
        Token.new(.R_PAREN, 627, 628), // )
        Token.new(.R_PAREN, 628, 629), // )
        Token.new(.R_PAREN, 629, 630), // )
        Token.new(.QUASI_QUOTE, 638, 639), // `
        Token.new(.L_PAREN, 639, 640), // (
        Token.new(.IDENTIFIER, 640, 643), // let
        Token.new(.IDENTIFIER, 644, 648), // loop
        Token.new(.L_PAREN, 649, 650), // (
        Token.new(.L_PAREN, 650, 651), // (
        Token.new(.UNQUOTE, 651, 652), // ,
        Token.new(.IDENTIFIER, 652, 660), // iterator
        Token.new(.NUMBER, 661, 662), // 0
        Token.new(.R_PAREN, 662, 663), // )
        Token.new(.L_PAREN, 687, 688), // (
        Token.new(.UNQUOTE, 688, 689), // ,
        Token.new(.IDENTIFIER, 689, 693), // name
        Token.new(.UNQUOTE, 694, 695), // ,
        Token.new(.IDENTIFIER, 695, 700), // value
        Token.new(.R_PAREN, 700, 701), // )
        Token.new(.R_PAREN, 701, 702), // )
        Token.new(.L_PAREN, 718, 719), // (
        Token.new(.IDENTIFIER, 719, 721), // if
        Token.new(.L_PAREN, 722, 723), // (
        Token.new(.IDENTIFIER, 723, 724), // <
        Token.new(.UNQUOTE, 725, 726), // ,
        Token.new(.IDENTIFIER, 726, 734), // iterator
        Token.new(.UNQUOTE, 735, 736), // ,
        Token.new(.IDENTIFIER, 736, 746), // iterations
        Token.new(.R_PAREN, 746, 747), // )
        Token.new(.L_PAREN, 763, 764), // (
        Token.new(.IDENTIFIER, 764, 768), // loop
        Token.new(.L_PAREN, 769, 770), // (
        Token.new(.IDENTIFIER, 770, 771), // +
        Token.new(.UNQUOTE, 772, 773), // ,
        Token.new(.IDENTIFIER, 773, 781), // iterator
        Token.new(.NUMBER, 782, 783), // 1
        Token.new(.R_PAREN, 783, 784), // )
        Token.new(.L_PAREN, 785, 786), // (
        Token.new(.IDENTIFIER, 786, 791), // begin
        Token.new(.UNQUOTE_SPLICING, 792, 794), // ,@
        Token.new(.IDENTIFIER, 794, 798), // body
        Token.new(.R_PAREN, 798, 799), // )
        Token.new(.R_PAREN, 799, 800), // )
        Token.new(.UNQUOTE, 816, 817), // ,
        Token.new(.IDENTIFIER, 817, 821), // name
        Token.new(.R_PAREN, 821, 822), // )
        Token.new(.R_PAREN, 822, 823), // )
        Token.new(.R_PAREN, 823, 824), // )
        Token.new(.R_PAREN, 824, 825), // )
        Token.new(.R_PAREN, 825, 826), // )
    }), .{ .filter_atmosphere = true });
}
