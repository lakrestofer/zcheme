const Parser = @import("../Parser.zig");
const Sexpr = Parser.Sexpr;
const std = @import("std");
const ta = std.testing.allocator;
const Lexer = @import("../Lexer.zig");
const Token = Lexer.Token;
const TokenKind = Lexer.TokenKind;
const ArrayList = std.ArrayList;
const Arena = std.heap.ArenaAllocator;

fn test_parser(input: []const u8, expected: []const Sexpr) !void {
    var arena = Arena.init(ta);
    defer arena.deinit();

    var lexer = Lexer.init(input);
    var tokens = ArrayList(Token).init(arena.allocator());
    while (lexer.nextToken()) |t| try tokens.append(t);

    var parser = Parser.init(input, tokens.items, arena.allocator());
    var sexprs = ArrayList(*Sexpr).init(arena.allocator());
    while (try parser.nextSexpr()) |e| try sexprs.append(e);
    try std.testing.expectEqual(sexprs.items.len, expected.len);

    for (sexprs.items, expected) |e, e2| {
        try std.testing.expect(std.meta.eql(e.*, e2));
    }
}

const Factory = Sexpr.Factory;
test "boolean" {
    try test_parser("#t #T #f #F", &[_]Sexpr{
        Factory.boolean(true),
        Factory.boolean(true),
        Factory.boolean(false),
        Factory.boolean(false),
    });
}
test "integer" {
    try test_parser(
        "98222 0xff 0xFF 0o755 0b11110000 1_000_000_000 0b1_1111_1111 0o7_5_5",
        &[_]Sexpr{
            Factory.integer(98222),
            Factory.integer(0xff),
            Factory.integer(0xFF),
            Factory.integer(0o755),
            Factory.integer(0b11110000),
            Factory.integer(1_000_000_000),
            Factory.integer(0b1_1111_1111),
            Factory.integer(0o7_5_5),
        },
    );
}
test "float" {
    try test_parser(
        "123.0E+77 123.0 123.0e+77 0x103.70p-5 0x103.70 0x103.70P-5 299_792_458.000_000 0.000_000_001 0x1234_5678.9ABC_CDEFp-10",
        &[_]Sexpr{
            Factory.float(123.0E+77),
            Factory.float(123.0),
            Factory.float(123.0e+77),
            Factory.float(0x103.70p-5),
            Factory.float(0x103.70),
            Factory.float(0x103.70P-5),
            Factory.float(299_792_458.000_000),
            Factory.float(0.000_000_001),
            Factory.float(0x1234_5678.9ABC_CDEFp-10),
        },
    );
}
fn testFile(comptime name: []const u8) !void {
    var arena = Arena.init(ta);
    defer arena.deinit();

    const cwd = std.fs.cwd();
    const in = try cwd.readFileAlloc(ta, std.fmt.comptimePrint("tests/{s}", .{name}), 1048576);
    const sexpr_expected = try cwd.readFileAlloc(ta, std.fmt.comptimePrint("tests/{s}.sexpr.expected", .{name}), 1048576);
    const sexpr_actual = try cwd.createFile(std.fmt.comptimePrint("tests/{s}.sexpr.actual", .{name}), .{});
    defer sexpr_actual.close();

    defer ta.free(in);
    defer ta.free(sexpr_expected);

    var lexer = Lexer.init(in);
    var tokens = ArrayList(Token).init(ta);
    defer tokens.deinit();
    while (lexer.nextToken()) |t| try tokens.append(t);

    var parser = Parser.init(in, tokens.items, arena.allocator());

    var sexpr_str = ArrayList(u8).init(ta);
    var writer = sexpr_str.writer();
    defer sexpr_str.deinit();
    while (try parser.nextSexpr()) |e| try writer.print("{}\n", .{e});

    try sexpr_actual.writer().writeAll(sexpr_str.items);

    if (sexpr_expected.len == 0) return error.file_empty;
    if (sexpr_str.items.len == 0) return error.file_empty;

    try std.testing.expectEqualSlices(u8, sexpr_expected, sexpr_str.items);
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
