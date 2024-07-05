const std = @import("std");
const testing = std.testing;

test "basic test" {
    const Lexer = @import("lexer").Lexer;
    var lexer = Lexer.init("()", .{});
    std.debug.print("{any}", .{lexer.nextToken()});
}
