/// Zcheme Parser. Parser slice of characters into Sexpressions
const std = @import("std");
const Lexer = @import("Lexer.zig");
const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;

// struct Parser {
/// the input sequence that is to be parsed
lexer: Lexer,
/// arena that will hold the allocations for the sexpressions
// }

const Self = @This();

pub fn init(arena: Arena, lexer: Lexer) Self {
    return Self{ arena, lexer };
}
