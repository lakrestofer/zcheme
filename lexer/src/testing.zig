const std = @import("std");
const test_allocator = std.testing.allocator;
const Lexer = @import("./Lexer.zig");
const Token = Lexer.Token;

/// test a single production rule that should match
/// it should match the input, and return it's length
pub fn test_prod(
    prod: *const fn (input: []const u8, pos: *usize) bool,
    input: []const u8,
    expected_end: usize,
) !void {
    var pos: usize = 0;
    try std.testing.expect(prod(input, &pos));
    try std.testing.expectEqual(expected_end, pos);
}

pub fn test_lexer(input: []const u8, expected: []const Token) !void {
    var lexer = Lexer.init(input);
    var tokens = std.ArrayList(Token).init(test_allocator);
    defer tokens.deinit();

    while (lexer.nextToken()) |token| try tokens.append(token);
    // for (tokens.items) |token| {
    //     std.debug.print("token: {any}\n", .{token.kind});
    // }
    try std.testing.expectEqualSlices(Token, expected, tokens.items);
}

test "(\n(\t)  )" {
    try test_lexer("(\n(\t)  )", &([_]Token{
        Token.new(.L_PAREN, 0, 1),
        Token.new(.WHITESPACE, 1, 2),
        Token.new(.L_PAREN, 2, 3),
        Token.new(.WHITESPACE, 3, 4),
        Token.new(.R_PAREN, 4, 5),
        Token.new(.WHITESPACE, 5, 7),
        Token.new(.R_PAREN, 7, 8),
        Token.new(.EOF, 8, 8),
    }));
}

test "#(())" {
    try test_lexer("#(())", &([_]Token{
        Token.new(.L_VEC_PAREN, 0, 2),
        Token.new(.L_PAREN, 2, 3),
        Token.new(.R_PAREN, 3, 4),
        Token.new(.R_PAREN, 4, 5),
        Token.new(.EOF, 5, 5),
    }));
}

test "multiline with comments" {
    const input =
        \\; this is a comment with a newline
        \\'() ; here is another with a manual newline \r\n
        \\;and a comment
    ;
    try test_lexer(input, &([_]Token{
        Token.new(.COMMMENT, 0, 35),
        Token.new(.QUOTE, 35, 36),
        Token.new(.L_PAREN, 36, 37),
        Token.new(.R_PAREN, 37, 38),
        Token.new(.WHITESPACE, 38, 39),
        Token.new(.COMMMENT, 39, 84),
        Token.new(.COMMMENT, 84, 98),
        Token.new(.EOF, 98, 98),
    }));
}
