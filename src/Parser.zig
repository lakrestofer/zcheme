/// Zcheme Parser. Parsers slice of tokens into S-expressions
const std = @import("std");
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;
const TokenKind = Lexer.TokenKind;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;

// struct Parser {
tokens: []const Token, // the input sequence that is to be parsed
input: []const u8,
pos: usize = 0,
arena: Allocator, // arena that will hold the allocations for the sexpressions
// }

const Self = @This();

pub const Sexpr = union(SexprKind) {
    nil,
    cons: struct { value: *Sexpr, next: *Sexpr },
    integer: i64,
    float: f64,
    string: []const u8,
    character: u8,
    boolean: bool,

    pub fn format(
        self: Sexpr,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .nil => writer.print("Nil", .{}),
            .cons => |c| {
                writer.print("Cons {{ {}, {} }}", .{ c.value, c.next });
            },
            .integer => |i| writer.print("Int {{ {} }}", .{i}),
            .float => |f| writer.print("Float {{ {} }}", .{f}),
            .string => |s| writer.print("String {{ {} }}", .{s}),
            .character => |c| writer.print("Char {{ {} }}", .{c}),
            .boolean => |b| writer.print("Bool {{ {} }}", .{b}),
            else => @panic("format not implemented for sexpr kind"),
        }
    }

    pub const Factory = struct {
        pub fn nil() Sexpr {
            return .nil;
        }
        pub fn cons(value: *Sexpr, next: *Sexpr) Sexpr {
            return Sexpr{ .cons = .{ .value = value, .next = next } };
        }
        pub fn integer(value: i64) Sexpr {
            return Sexpr{ .integer = value };
        }
        pub fn float(value: f64) Sexpr {
            return Sexpr{ .float = value };
        }
        pub fn string(value: []const u8) Sexpr {
            return Sexpr{ .string = value };
        }
        pub fn character(value: []const u8) Sexpr {
            return Sexpr{ .character = value };
        }
        pub fn boolean(value: bool) Sexpr {
            return Sexpr{ .boolean = value };
        }
    };
};

pub const SexprKind = enum {
    nil,
    cons,
    integer,
    float,
    string,
    character,
    boolean,
};

pub const Error = error{
    SyntaxError,
};

pub fn init(input: []const u8, tokens: []const Token, arena: Allocator) Self {
    return Self{ .input = input, .tokens = tokens, .arena = arena };
}

pub fn nextSexpr(self: *Self) anyerror!?*Sexpr {
    if (self.pos >= self.tokens.len) return null;

    if (try self.datum()) |e| return e;

    self.pos += 1; // always advance the cursor such that we may terminate

    // matched no datum rule
    return error.SyntaxError;
}

fn datum(self: *Self) !?*Sexpr {
    // we have the lexeme datums
    if (try self.boolean()) |s| return s;
    if (try self.integer()) |s| return s;
    if (try self.float()) |s| return s;
    // if (try self.char()) |s| return s;
    // if (try self.string()) |s| return s;
    // if (try self.symbol()) |s| return s;
    // and the compound datum
    // if (try self.list()) |s| return s;
    // if (try self.vector()) |s| return s;
    // if (try self.byte_vector()) |s| return s;
    return null;
}

fn boolean(self: *Self) !?*Sexpr {
    var p = self.pos;
    if (self.expect(TokenKind.boolean, &p)) |b| {
        const c = self.input[b.start + 1]; // we check the second character
        const value = (c == 't' or c == 'T');
        const expr: *Sexpr = try self.arena.create(Sexpr);
        expr.* = Sexpr{ .boolean = value };
        self.pos = p;
        return expr;
    }
    return null;
}

fn integer(self: *Self) !?*Sexpr {
    var p = self.pos;
    if (self.expect(TokenKind.integer, &p)) |t| {
        const value: i64 = std.fmt.parseInt(i64, self.input[t.start..t.end], 0) catch @panic("could not parse integer token as i64");
        const expr: *Sexpr = try self.arena.create(Sexpr);
        expr.* = Sexpr{ .integer = value };
        self.pos = p;
        return expr;
    }
    return null;
}
fn float(self: *Self) !?*Sexpr {
    var p = self.pos;
    if (self.expect(TokenKind.float, &p)) |t| {
        const value: f64 = std.fmt.parseFloat(f64, self.input[t.start..t.end]) catch @panic("could not parse integer token as i64");
        const expr: *Sexpr = try self.arena.create(Sexpr);
        expr.* = Sexpr{ .float = value };
        self.pos = p;
        return expr;
    }
    return null;
}

const NameValuePair = std.meta.Tuple(&.{ []const u8, u8 });
const CHAR_NAME_VALUE_MAP = std.StaticStringMap(u8).initComptime(&[_]NameValuePair{
    .{ "nul", '\x00' },
    .{ "alarm", '\x07' },
    .{ "backspace", '\x08' },
    .{ "tab", '\x09' },
    .{ "linefeed", '\x0A' },
    .{ "newline", '\x0A' },
    .{ "vtab", '\x0B' },
    .{ "page", '\x0C' },
    .{ "return", '\x0D' },
    .{ "esc", '\x1B' },
    .{ "space", ' ' },
    .{ "delete", '\x7F' },
});

fn char(self: *Self) !?*Sexpr {
    var p = self.pos;
    if (self.expect(TokenKind.character, &p)) |t| {
        const c_str = self.input[(t.start + 2)..t.end]; // slice after "#\"

        const value = value: {
            // if the char is a named literal return its corresponding value
            if (CHAR_NAME_VALUE_MAP.get(c_str)) |val| break :value val;

            if (c_str[0] == 'x' and c_str.len > 1) {
                // hex literal
                break :value try std.fmt.parseInt(u8, c_str[1..], 16);
            }

            if (c_str.len == 1) break :value c_str[0];

            unreachable;
        };

        const expr: *Sexpr = try self.arena.create(Sexpr);
        expr.character = value;
        self.pos = p;
        return expr;
    }
    return null;
}
fn string(self: *Self) !?*Sexpr {
    _ = self; // autofix
    return null;
}
fn symbol(self: *Self) !?*Sexpr {
    _ = self; // autofix
    return null;
}

fn list(self: *Self) !?*Sexpr {
    _ = self; // autofix
    return null;
}
fn vector(self: *Self) !?*Sexpr {
    _ = self; // autofix
    return null;
}
fn byte_vector(self: *Self) !?*Sexpr {
    _ = self; // autofix
    return null;
}

// utils

fn expect(self: *Self, expected: TokenKind, pos: *usize) ?*const Token {
    if (self.tokens.len <= pos.*) return null;
    if (self.tokens[pos.*].kind != expected) return null;
    const token = &self.tokens[pos.*];
    pos.* += 1;
    return token;
}

fn expectSequence(self: *Self, expected: []const TokenKind, pos: *usize) bool {
    if (self.tokens.len <= pos.*) return null; // return null if the cursor points outside the tokens
    if (self.tokens[pos.*..].len < expected.len) return null; // or if there are not enough tokens left
    for (self.tokens[pos.*..], expected) |t1, t2| if (t1.kind != t2) return null; // or if any of the tokens are incorrect
    // otherwise we have a match!, and we progress the cursor
    pos.* += expected.len;
    return true;
}
