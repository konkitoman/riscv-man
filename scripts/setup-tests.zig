//usr/bin/env zig run -lc "$0" -- "$@"; exit

const std = @import("std");
const path = std.fs.path;
const utils = @import("utils/default.zig");
const print = std.debug.print;
const Allocator = std.mem.Allocator;

pub fn main() !void {
    try utils.check_root();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try utils.setup_gnu_toolchain(gpa.allocator());

    try prepare(gpa.allocator());
    try install(gpa.allocator());

    print("Official test ready, run `run-official-tests`\n", .{});
}

fn prepare(allocator: Allocator) !void {
    if (!utils.exists("official-riscv-tests")) {
        try utils.git_clone(allocator, "https://github.com/riscv-software-src/riscv-tests.git", "official-riscv-tests", .{ .recursive = true });
    }
}

fn install(allocator: Allocator) !void {
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
