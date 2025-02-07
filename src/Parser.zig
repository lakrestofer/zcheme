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
p: usize = 0,
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
            .nil => try writer.print("Nil", .{}),
            .cons => |c| {
                try writer.print("Cons {{ {}, {} }}", .{ c.value, c.next });
            },
            .integer => |i| try writer.print("Int {{ {} }}", .{i}),
            .float => |f| try writer.print("Float {{ {} }}", .{f}),
            .string => |s| try writer.print("String {{ {s} }}", .{s}),
            .character => |c| try writer.print("Char {{ {} }}", .{c}),
            .boolean => |b| try writer.print("Bool {{ {} }}", .{b}),
            // else => @panic("format not implemented for sexpr kind"),
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

pub const Error = error{SyntaxError} || Allocator.Error;

pub fn init(input: []const u8, tokens: []const Token, arena: Allocator) Self {
    return Self{ .input = input, .tokens = tokens, .arena = arena };
}

pub fn nextSexpr(self: *Self) Error!?*Sexpr {
    if (self.p >= self.tokens.len) return null;

    var p = self.p;
    if (try self.datum(&p)) |e| {
        self.p = p;
        return e;
    }

    self.p += 1; // always advance the cursor such that we may terminate

    // matched no datum rule
    return error.SyntaxError;
}

fn datum(self: *Self, p: *usize) Error!?*Sexpr {
    // we have the lexeme datums
    if (try self.boolean(p)) |s| return s;
    if (try self.integer(p)) |s| return s;
    if (try self.float(p)) |s| return s;
    if (try self.char(p)) |s| return s;
    // if (try self.string()) |s| return s;
    // if (try self.symbol()) |s| return s;
    // and the compound datum
    if (try self.list(p)) |s| return s;
    // if (try self.vector()) |s| return s;
    // if (try self.byte_vector()) |s| return s;
    return null;
}

fn boolean(self: *Self, p_: *usize) Error!?*Sexpr {
    var p = p_.*;
    if (self.expect(TokenKind.boolean, &p)) |b| {
        const c = self.input[b.start + 1]; // we check the second character
        const value = (c == 't' or c == 'T');
        const expr: *Sexpr = try self.arena.create(Sexpr);
        expr.* = Sexpr{ .boolean = value };
        p_.* = p;
        return expr;
    }
    return null;
}

fn integer(self: *Self, p_: *usize) Error!?*Sexpr {
    var p = p_.*;
    if (self.expect(TokenKind.integer, &p)) |t| {
        _ = t; // autofix
        // const value: i64 = try std.fmt.parseInt(i64, self.input[t.start..t.end], 0) catch @panic("could not parse integer token as i64");
        const value: i64 = 42;
        const expr: *Sexpr = try self.arena.create(Sexpr);
        expr.* = Sexpr{ .integer = value };
        p_.* = p;
        return expr;
    }
    return null;
}
fn float(self: *Self, p_: *usize) Error!?*Sexpr {
    var p = p_.*;
    if (self.expect(TokenKind.float, &p)) |t| {
        const value: f64 = std.fmt.parseFloat(f64, self.input[t.start..t.end]) catch @panic("could not parse integer token as i64");
        const expr: *Sexpr = try self.arena.create(Sexpr);
        expr.* = Sexpr{ .float = value };
        p_.* = p;
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

fn char(self: *Self, p_: *usize) Error!?*Sexpr {
    var p = p_.*;
    if (self.expect(TokenKind.character, &p)) |t| {
        const c_str = self.input[(t.start + 2)..t.end]; // slice after "#\"

        const value = value: {
            // if the char is a named literal return its corresponding value
            if (CHAR_NAME_VALUE_MAP.get(c_str)) |val| break :value val;

            if (c_str[0] == 'x' and c_str.len > 1) {
                // hex literal
                // break :value try std.fmt.parseInt(u8, c_str[1..], 16);
                break :value 'x';
            }

            if (c_str.len == 1) break :value c_str[0];

            unreachable;
        };

        const expr: *Sexpr = try self.arena.create(Sexpr);
        expr.character = value;
        p_.* = p;
        return expr;
    }
    return null;
}
fn string(self: *Self, p: *usize) Error!?*Sexpr {
    _ = p; // autofix
    _ = self; // autofix
    return null;
}
fn symbol(self: *Self, p: *usize) Error!?*Sexpr {
    _ = p; // autofix
    _ = self; // autofix
    return null;
}

fn list(self: *Self, p_: *usize) Error!?*Sexpr {
    var p = p_.*;
    // (<datum>*)
    // (<datum>+ . <datum>)
    if (self.expect(.l_paren, &p)) |_| {} else return null;

    var datums_list = ArrayList(*Sexpr).init(self.arena);
    defer datums_list.deinit();
    _ = value: {
        while (try self.datum(&p)) |d| try datums_list.append(d);

        if (self.expect(.period, &p)) |_| {
            if (datums_list.items.len > 0) {
                // at least one datum and a period followed by one
                if (try self.datum(&p)) |e| {
                    try datums_list.append(e);
                    break :value void;
                }
            }
        }
        break :value void;
    };

    if (self.expect(.r_paren, &p)) |_| {} else return null;

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
