const std = @import("std");

pub fn build(b: *std.Build) void {
    // == build options ==
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // === build library ===
    // build lexer
    const lexer = b.dependency("lexer", .{
        .target = target,
        .optimize = optimize,
    });
    const lm = lexer.module("lexer");
    // build sexpr library
    const lib = b.addStaticLibrary(.{
        .name = "sexpr",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    lib.root_module.addImport("lexer", lm);
    b.installArtifact(lib);
    _ = b.addModule("sexpr", .{ .root_source_file = lib.root_module.root_source_file });

    // === build tests ===
    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    lib_unit_tests.root_module.addImport("lexer", lm);
    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    // === steps ===
    // zig build test
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
}
