const std = @import("std");
const test_allocator = std.testing.allocator;
const Lexer = @import("../Lexer.zig");
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

pub fn test_token(
    input: []const u8,
    expected_token: Token,
) !void {
    var lexer = Lexer.init(input, .{});
    const token = lexer.nextToken() orelse return error.NoTokenError;
    try std.testing.expectEqual(expected_token, token);
}

// for the productions that require to be parameterized with
// a comptime_int
pub fn test_prod_base(
    prod: *const fn (base: comptime_int, input: []const u8, pos: *usize) bool,
    base: comptime_int,
    input: []const u8,
    expected_end: usize,
) !void {
    var pos: usize = 0;
    try std.testing.expect(prod(base, input, &pos));
    try std.testing.expectEqual(expected_end, pos);
}

pub fn test_lexer(input: []const u8, expected: []const Token, options: Lexer.LexerOptions) !void {
    var lexer = Lexer.init(input, options);
    var tokens = std.ArrayList(Token).init(test_allocator);
    defer tokens.deinit();

    while (lexer.nextToken()) |token| {
        try tokens.append(token);
    }
    try std.testing.expectEqualSlices(Token, expected, tokens.items);
}

test {
    // most of the syntax tests are in this file
    _ = @import("./syntax.zig");
}
