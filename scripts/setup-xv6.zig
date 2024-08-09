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
    try build(gpa.allocator());

    print("xv6 is ready!\n", .{});
}

fn prepare(allocator: Allocator) !void {
    if (!utils.exists("xv6")) {
        try utils.git_clone(allocator, "https://github.com/mit-pdos/xv6-riscv", "xv6", .{});
    }
}

fn build(allocator: Allocator) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    try std.process.changeCurDir("xv6");

    try utils.run(alloc, &.{ "make", "-j32" });

    try std.process.changeCurDir("..");
}
