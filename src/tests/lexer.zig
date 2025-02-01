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

fn testFile(comptime name: []const u8) !void {
    const cwd = std.fs.cwd();
    const in = try cwd.readFileAlloc(ta, std.fmt.comptimePrint("tests/{s}", .{name}), 1048576);
    const tokens_expected = try cwd.readFileAlloc(ta, std.fmt.comptimePrint("tests/{s}.tokens.expected", .{name}), 1048576);
    const tokens_actual = try cwd.createFile(std.fmt.comptimePrint("tests/{s}.tokens.actual", .{name}), .{});
    defer tokens_actual.close();

    defer ta.free(in);
    defer ta.free(tokens_expected);

    var lexer = Lexer.init(in);
    var tokens_str = ArrayList(u8).init(ta);
    var writer = tokens_str.writer();
    defer tokens_str.deinit();
    while (lexer.nextToken()) |t| try writer.print("{}\n", .{t});

    try tokens_actual.writer().writeAll(tokens_str.items);

    try std.testing.expectEqualSlices(u8, tokens_expected, tokens_str.items);
}

comptime {
    // we generate test cases for each sample test file
    for ([_][]const u8{
        "simple.ss",
    }) |name| {
        _ = struct {
            test {
                try testFile(name);
            }
        };
    }
}

test "fuzzing" {
    try std.testing.fuzz(fuzz_lexer, .{});
}
