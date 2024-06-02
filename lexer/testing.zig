pub fn test_lexer(input: []const u8, expected: []const Token) !void {
    var lexer = Self.init(input);
    for (expected) |token| {
        try std.testing.expectEqual(token, lexer.nextToken());
    }
}
