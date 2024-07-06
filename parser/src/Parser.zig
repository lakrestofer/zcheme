// std lib imports
const std = @import("std");
const ArrayList = std.ArrayList;
const TokenArrayList = ArrayList(Lexer.Token);
const Allocator = std.mem.Allocator;
// zcheme imports
const Lexer = @import("lexer").Lexer;
const Token = Lexer.Token;
const Sexpr = @import("sexpr").Sexpr;

// struct Parser {
tokens: ArrayList(Lexer.Token),
pos: usize = 0,
// }

const Self = @This();

pub fn init(lexer: Lexer, alloc: Allocator) Self {
    const ts = TokenArrayList.init(alloc);
    while (lexer.nextToken()) |t| ts.append(t);
    return Self{ .tokens = ts };
}
pub fn deinit(self: *Self) void {
    self.tokens.deinit();
}

pub fn nextSexpr(self: *Self) ?Sexpr {
    if (lexeme_datum(self.tokens.items, &self.pos)) |token| return token;
    if (compound_datum(self.tokens.items, &self.pos)) |token| return token;
    return null;
}

fn lexeme_datum(tokens: []Token, pos: *usize) ?Sexpr {
    const token = tokens[pos.*];
}
