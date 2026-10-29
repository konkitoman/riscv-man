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
    try build(init.io, init.environ_map);

    print("xv6 is ready!\n", .{});
}

fn prepare(allocator: Allocator, io: std.Io) !void {
    if (!utils.exists(io, "xv6")) {
        try utils.git_clone(allocator, io, "https://github.com/mit-pdos/xv6-riscv", "xv6", .{});
    }
}

fn build(io: std.Io, environ: *std.process.Environ.Map) !void {
    try std.process.setCurrentPath(io, "xv6");

    try utils.run(io, environ, &.{ "make", "-j32" });

    try std.process.setCurrentPath(io, "..");
}
