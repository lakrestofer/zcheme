//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
pub const Parser = @import("Parser.zig");
pub const Lexer = @import("Lexer.zig");

test {
    _ = @import("Lexer.zig");
    _ = @import("tests/lexer.zig");
    _ = @import("Parser.zig");
    _ = @import("tests/parser.zig");
}
