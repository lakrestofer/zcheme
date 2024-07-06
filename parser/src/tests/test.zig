const std = @import("std");

test "fail" {
    try std.testing.expect(false);
}
