const std = @import("std");
const Lexer = @import("../Lexer.zig");
const TokenKind = Lexer.TokenKind;
const ArrayList = std.ArrayList;
const ta = std.testing.allocator;

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

fn test_lexer_token_kind(input: []const u8, expected: []const TokenKind) !void {
    var lexer = Lexer.init(input);
    var actual = ArrayList(TokenKind).init(ta);
    defer actual.deinit();
    while (lexer.nextToken()) |t| try actual.append(t.kind);
    try std.testing.expectEqualSlices(TokenKind, expected, actual.items);
}

test "simple_expr" {
    try test_lexer_token_kind("()()()", &[_]TokenKind{
        .l_paren,
        .r_paren,
        .l_paren,
        .r_paren,
        .l_paren,
        .r_paren,
    });
}

fn fuzz_lexer(input: []const u8) anyerror!void {
    var lexer = Lexer.init(input);
    var actual = ArrayList(TokenKind).init(ta);
    defer actual.deinit();
    while (lexer.nextToken()) |t| try actual.append(t.kind);
}

test "fuzzing" {
    try std.testing.fuzz(fuzz_lexer, .{});
}
