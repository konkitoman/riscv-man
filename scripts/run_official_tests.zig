//usr/bin/env zig run -lc "$0" -- "$@"; exit

const std = @import("std");
const utils = @import("utils/default.zig");
const print = std.debug.print;
const Allocator = std.mem.Allocator;

pub fn main() !void {
    try utils.check_root();

    var args = std.process.args();
    const path = args.next();
    _ = path;

    const cpu_meta = if (args.next()) |bin| bin else {
        print("cpu meta is required!\n", .{});
        return;
    };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const alloc = arena.allocator();

    try utils.run(alloc, &.{ "zig", "build" });

    const cwd = try std.fs.cwd().realpathAlloc(alloc, ".");
    const test_runner_path = try std.fs.cwd().realpathAlloc(alloc, "zig-out/bin/riscv-man-test-runner");
    var dir = try std.fs.cwd().openDir(try std.fs.path.join(alloc, &.{ "local", "share", "riscv-tests", "isa" }), .{ .iterate = true });
    defer dir.close();
    var dir_iter = dir.iterate();

    var failed: usize = 0;
    var tests: usize = 0;

    while (try dir_iter.next()) |entry| {
        if (entry.kind != .file) {
            continue;
        }
        if (entry.name[0] == '.' or std.mem.endsWith(u8, entry.name, ".dump") or std.mem.eql(u8, entry.name, "Makefile")) {
            continue;
        }

        if (std.mem.eql(u8, cpu_meta[0..4], "RV32")) {
            if (std.mem.startsWith(u8, entry.name, "rv64")) {
                continue;
            }
        }

        tests += 1;

        const program = try std.fs.path.join(alloc, &.{ cwd, "local", "share", "riscv-tests", "isa", entry.name });
        defer alloc.free(program);

        print("Test: {s}\n", .{entry.name});

        if (std.process.Child.run(.{ .allocator = alloc, .argv = &.{ "timeout", "0.1s", test_runner_path, cpu_meta, program } })) |result| {
            switch (result.term) {
                .Exited => |code| {
                    if (code == 0) {
                        continue;
                    }
                },
                else => {},
            }
            failed += 1;
            print("Stdout: {s}\n", .{result.stdout});
            print("Stderr: {s}\n", .{result.stderr});
        } else |_| {}
    }

    print("{d} tests runned!\n", .{tests});
    if (failed != 0) {
        print("{d} tests failed!\n", .{failed});
        std.process.exit(1);
    }
}