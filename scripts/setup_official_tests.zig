//usr/bin/env zig run -lc "$0" -- "$@"; exit

const std = @import("std");
const path = std.fs.path;
const utils = @import("utils/default.zig");
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const c = @cImport({
    @cInclude("stdlib.h");
});

pub fn main() !void {
    try utils.check_root();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    if (utils.exists("riscv-gnu-toolchain") or utils.exists("official-riscv-tests") or utils.exists("local")) {
        print("This is not clean!!! you can to run `clean_official_tests.zig`\n", .{});
    }

    try prepare_toolchain(gpa.allocator());
    try install_toolchain(gpa.allocator());

    try prepare_tests(gpa.allocator());
    try install_tests(gpa.allocator());

    print("Official test ready, run `run-official-tests`\n", .{});
}

fn prepare_toolchain(allocator: Allocator) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    if (!utils.exists("riscv-gnu-toolchain")) {
        try utils.git_clone(alloc, "https://github.com/riscv-collab/riscv-gnu-toolchain.git", "riscv-gnu-toolchain", .{});
    }

    if (!utils.exists("local")) {
        try std.fs.cwd().makeDir("local");
    }
}

fn install_toolchain(allocator: Allocator) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const cwd = try std.fs.cwd().realpathAlloc(alloc, ".");
    const toolchain_path = try path.join(alloc, &.{ cwd, "local" });

    try std.process.changeCurDir("riscv-gnu-toolchain");

    try utils.run(alloc, &.{ "./configure", "--prefix", toolchain_path });
    try utils.run(alloc, &.{ "make", "-j32" });

    try std.process.changeCurDir("..");
}

extern fn putenv(string: [*]const u8) i32;

fn prepare_tests(allocator: Allocator) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    if (!utils.exists("official-riscv-tests")) {
        try utils.git_clone(alloc, "https://github.com/riscv-software-src/riscv-tests.git", "official-riscv-tests", .{ .recursive = true });
    }

    const cwd = try std.fs.cwd().realpathAlloc(alloc, ".");
    const toolchain_path = try path.join(alloc, &.{ cwd, "local" });

    const LD_LIBRARY_PATH = std.process.getEnvVarOwned(alloc, "LD_LIBRARY_PATH") catch "";
    const NEW_LD_LIBRARY_PATH = try std.fmt.allocPrint(alloc, "{s}:{s}\x00", .{ LD_LIBRARY_PATH, try path.join(alloc, &.{ toolchain_path, "lib" }) });
    try setenv("LD_LIBRARY_PATH", NEW_LD_LIBRARY_PATH);

    const PATH = try std.process.getEnvVarOwned(alloc, "PATH");
    const NEW_PATH = try std.fmt.allocPrint(alloc, "{s}:{s}\x00", .{ PATH, try path.join(alloc, &.{ toolchain_path, "bin" }) });
    try setenv("PATH", NEW_PATH);
}

fn install_tests(allocator: Allocator) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const cwd = try std.fs.cwd().realpathAlloc(alloc, ".");
    const toolchain_path = try path.join(alloc, &.{ cwd, "local" });

    try std.process.changeCurDir("official-riscv-tests");

    try utils.run(alloc, &.{ "./configure", "--prefix", toolchain_path });
    try utils.run(alloc, &.{ "make", "install", "-j32" });

    try std.process.changeCurDir("..");
}

fn setenv(name: []const u8, value: []const u8) !void {
    if (c.setenv(name.ptr, value.ptr, 1) != 0) {
        return error.CannotSetEnv;
    }
}
