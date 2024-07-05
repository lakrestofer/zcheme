pub const Sexpr = union(enum) {
    Nil,
    Cons: struct {
        value: *Sexpr,
        link: *Sexpr,
    },
    Number: union(enum) { float: f64, integer: i64 },
    Identifier: struct { value: []u8 },
    String: struct { value: []u8 },
    Char: struct { value: u8 },
    Boolean: struct { value: bool },
};

test {
    _ = @import("./tests/test.zig");
}
