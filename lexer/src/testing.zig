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

pub fn test_lexer(input: []const u8, expected: []const Token, options: Lexer.LexerOptions) !void {
    var lexer = Lexer.init(input, options);
    var tokens = std.ArrayList(Token).init(test_allocator);
    defer tokens.deinit();

    while (lexer.nextToken()) |token| try tokens.append(token);
    try std.testing.expectEqualSlices(Token, expected, tokens.items);
}

test "(\n(\t)  )" {
    try test_lexer("(\n(\t)  )", &([_]Token{
        Token.new(.L_PAREN, 0, 1),
        Token.new(.L_PAREN, 2, 3),
        Token.new(.R_PAREN, 4, 5),
        Token.new(.R_PAREN, 7, 8),
    }), .{ .filter_atmosphere = true });
}

test "#(())" {
    try test_lexer("#(())", &([_]Token{
        Token.new(.L_VEC_PAREN, 0, 2),
        Token.new(.L_PAREN, 2, 3),
        Token.new(.R_PAREN, 3, 4),
        Token.new(.R_PAREN, 4, 5),
    }), .{ .filter_atmosphere = true });
}

test "multiline with comments" {
    const input =
        \\; this is a comment with a newline
        \\'() ; here is another with a manual newline \r\n
        \\;and a comment
    ;
    try test_lexer(input, &([_]Token{
        Token.new(.COMMENT, 0, 35),
        Token.new(.QUOTE, 35, 36),
        Token.new(.L_PAREN, 36, 37),
        Token.new(.R_PAREN, 37, 38),
        Token.new(.WHITESPACE, 38, 39),
        Token.new(.COMMENT, 39, 84),
        Token.new(.COMMENT, 84, 98),
    }), .{});
}
