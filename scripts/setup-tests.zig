//usr/bin/env zig run -lc "$0" -- "$@"; exit

const std = @import("std");
const path = std.fs.path;
const utils = @import("utils/default.zig");
const print = std.debug.print;
const Allocator = std.mem.Allocator;

pub fn main(init: std.process.Init) !void {
    try utils.check_root(init.io);

    try utils.setup_gnu_toolchain(init.gpa, init.io, init.environ_map);

    try prepare(init.gpa, init.io);
    try install(init.gpa, init.io, init.environ_map);

    print("Official test ready, run `run-tests`\n", .{});
}

fn prepare(allocator: Allocator, io: std.Io) !void {
    if (!utils.exists(io, "official-riscv-tests")) {
        try utils.git_clone(allocator, io, "https://github.com/riscv-software-src/riscv-tests.git", "official-riscv-tests", .{ .recursive = true });
    }
}

fn install(allocator: Allocator, io: std.Io, environ: *std.process.Environ.Map) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const cwd = try std.Io.Dir.cwd().realPathFileAlloc(io, ".", alloc);
    const toolchain_path = try path.join(alloc, &.{ cwd, "local" });

    try std.process.setCurrentPath(io, "official-riscv-tests");

    try utils.run(io, environ, &.{ "./configure", "--prefix", toolchain_path });
    try utils.run(io, environ, &.{ "make", "install", "-j32" });

    try std.process.setCurrentPath(io, "..");
}
