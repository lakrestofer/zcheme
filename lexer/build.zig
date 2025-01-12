const std = @import("std");

pub fn build(b: *std.Build) void {
    // == build options ==
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // === build library ===
    const lib = b.addStaticLibrary(.{
        .name = "lexer",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(lib);
    // export this library to a consumer of the lexer package
    _ = b.addModule("lexer", .{ .root_source_file = lib.root_module.root_source_file });

    // === build lexer executable ===
    // simple toy program that spits out sequence of tokens
    const exe = b.addExecutable(.{
        .name = "lexer",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(exe);

    // === build tests ===
    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    // === create commands ===
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);

    // === steps ===
    // zig build run
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
    // zig build test
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
}
