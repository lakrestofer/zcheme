const std = @import("std");
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
    for (expected) |token| {
        try std.testing.expectEqual(token, lexer.nextToken());
    }
}
