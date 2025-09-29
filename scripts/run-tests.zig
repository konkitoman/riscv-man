//usr/bin/env zig run -lc "$0" -- "$@"; exit

const std = @import("std");
const utils = @import("utils/default.zig");
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const Task = struct {
    name: []const u8,
    process: std.process.Child,
    stdout: std.ArrayList(u8),
    id: u32,
};

fn handle_test(pipe: std.posix.fd_t, task: *Task, alloc: Allocator) void {
    if (task.process.spawn()) |_| {} else |_| {}
    if (!(std.process.hasEnvVar(alloc, "SIMPLE") catch false)) {
        if (task.process.collectOutput(alloc, &task.stdout, &task.stdout, std.math.maxInt(usize))) {} else |_| {}
    }
    if (task.process.wait()) |term| {
        task.process.term = term;
    } else |_| {}
    task.process.progress_node.end();

    var buffer: [4]u8 = undefined;
    std.mem.writeInt(u32, &buffer, task.id, .little);
    if (std.posix.write(pipe, &buffer)) |_| {} else |_| {}
}

pub fn main() !void {
    try utils.check_root();

    var args = std.process.args();
    const path = args.next();
    _ = path;

    const filter = if (args.next()) |bin| bin else "";

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    var thread_safe_allocator = std.heap.ThreadSafeAllocator{ .child_allocator = arena.allocator() };
    const alloc = thread_safe_allocator.allocator();

    print("Building the test runner:\n", .{});
    try utils.run(alloc, &.{ "zig", "build", "test-runner" });

    const cwd = try std.fs.cwd().realpathAlloc(alloc, ".");
    const test_runner_path = try std.fs.cwd().realpathAlloc(alloc, "zig-out/bin/rvman-test-runner");
    var dir = try std.fs.cwd().openDir(try std.fs.path.join(alloc, &.{ "local", "share", "riscv-tests", "isa" }), .{ .iterate = true });
    defer dir.close();
    var dir_iter = dir.iterate();

    var tests = std.ArrayList(*Task){};

    print("\nRunning tests:\n", .{});
    const progress = std.Progress.start(.{});

    const pipes = try std.posix.pipe();
    const efd = try std.posix.epoll_create1(0);

    {
        var event: std.os.linux.epoll_event = .{ .events = std.os.linux.EPOLL.IN, .data = .{ .u32 = 1 } };
        std.debug.assert(std.os.linux.epoll_ctl(efd, std.os.linux.EPOLL.CTL_ADD, pipes[0], &event) == 0);
    }

    while (try dir_iter.next()) |entry| {
        if (entry.kind != .file) {
            continue;
        }
        if (entry.name[0] == '.' or std.mem.endsWith(u8, entry.name, ".dump") or std.mem.eql(u8, entry.name, "Makefile")) {
            continue;
        }

        if (filter.len != 0 and !std.mem.containsAtLeast(u8, entry.name, 1, filter)) {
            continue;
        }

        var cpu_meta: []const u8 = "";

        if (std.mem.startsWith(u8, entry.name, "rv32")) {
            cpu_meta = "RV32";
        } else if (std.mem.startsWith(u8, entry.name, "rv64")) {
            cpu_meta = "RV64";
        } else {
            continue;
        }

        const program = try std.fs.path.join(alloc, &.{ cwd, "local", "share", "riscv-tests", "isa", entry.name });

        const task = try alloc.create(Task);
        const last_len: u32 = @truncate(tests.items.len);
        try tests.append(alloc, task);
        const name = try alloc.dupe(u8, entry.name);
        const argv = try alloc.dupe([]const u8, &.{ test_runner_path, cpu_meta, program });

        task.* = .{
            .id = last_len + 1,
            .name = name,
            .process = std.process.Child.init(argv, alloc),
            .stdout = .{},
        };
        if (!try std.process.hasEnvVar(alloc, "SIMPLE")) {
            task.process.stdout_behavior = .Pipe;
            task.process.stderr_behavior = .Pipe;
        } else {
            task.process.stdout_behavior = .Close;
            task.process.stderr_behavior = .Close;
        }
        task.process.progress_node = progress.start(entry.name, 0);
        _ = try std.Thread.spawn(.{ .allocator = alloc }, handle_test, .{ pipes[1], task, alloc });
    }

    var events: [1]std.os.linux.epoll_event = undefined;

    var finished: usize = 0;
    while (tests.items.len != 0) {
        const result = std.os.linux.epoll_wait(efd, &events, 1, -1);
        if (result != 1) continue;
        if (events[0].data.u32 == 1) {
            var buffer: [4]u8 = undefined;
            std.debug.assert(try std.posix.read(pipes[0], &buffer) == 4);
            const id = std.mem.readInt(u32, &buffer, .little);
            const task = tests.items[id - 1];
            std.debug.assert(task.id == id);
            finished += 1;

            if (finished == tests.items.len) {
                break;
            }
            continue;
        }
    }

    progress.end();

    var failed: usize = 0;
    var passed: usize = 0;

    for (tests.items) |task| {
        if (task.process.term) |result| {
            if (result) |term| {
                switch (term) {
                    .Exited => |code| {
                        if (code == 0) {
                            passed += 1;
                            print("\x1b[32mPass\x1B[0m {s}\n", .{task.name});
                            continue;
                        }
                    },
                    else => {},
                }
            } else |_| {}
        } else {
            print("\tNo\n", .{});
        }
        failed += 1;

        if (!try std.process.hasEnvVar(alloc, "SIMPLE")) {
            print("\x1B[91mFailed\x1B[0m {s}\n", .{task.name});
            print("{s}", .{task.stdout.items});
        }
        print("\x1B[91mFailed\x1B[0m {s}\n", .{task.name});
    }
    print("\n{d} tests runned!\n", .{tests.items.len});
    print("\x1B[91m{d} tests failed!\x1B[0m\n", .{failed});
    print("\x1B[32m{d} tests passed!\x1B[0m\n", .{tests.items.len - failed});
    if (failed != 0) {
        const fails: f32 = @floatFromInt(failed);
        const testss: f32 = @floatFromInt(tests.items.len);
        print("Pass rate: {d:.2}%\n", .{((testss - fails) / testss) * 100.0});
        print("Fail rate: {d:.2}%\n", .{(fails / testss) * 100.0});
        std.process.exit(1);
    }
    print("All tests passed!!!\n", .{});
}
