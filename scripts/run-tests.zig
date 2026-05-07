//usr/bin/env zig run -lc "$0" -- "$@"; exit

const std = @import("std");
const utils = @import("utils/default.zig");
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const Task = struct {
    name: []const u8,
    process: std.process.Child,
    stdout: std.ArrayList(u8),
    alloc: Allocator,
    term: ?std.process.Child.Term = null,
    progress_node: std.Progress.Node,
    id: u32,
};

fn handle_test(io: std.Io, queue: *std.Io.Queue(u32), task: *Task) void {
    if (task.process.stdout != null and task.process.stderr != null) {
        const stdout = task.process.stdout.?;
        const stderr = task.process.stderr.?;
        var stdout_buffer: [1024]u8 = undefined;
        const stdout_buffers: [1][]u8 = .{&stdout_buffer};
        var stderr_buffer: [1024]u8 = undefined;
        const stderr_buffers: [1][]u8 = .{&stderr_buffer};
        const select_variant = union(enum) {
            stdout: std.Io.File.ReadStreamingError!usize,
            stderr: std.Io.File.ReadStreamingError!usize,
        };
        var select_buffer: [2]select_variant = undefined;

        var select = std.Io.Select(select_variant).init(io, &select_buffer);
        select.concurrent(.stdout, std.Io.File.readStreaming, .{ stdout, io, &stdout_buffers }) catch {
            std.log.err("No concurrency!", .{});
        };
        select.concurrent(.stderr, std.Io.File.readStreaming, .{ stderr, io, &stderr_buffers }) catch {
            std.log.err("No concurrency!", .{});
        };

        while (true) {
            const result = select.await() catch {
                continue;
            };

            switch (result) {
                .stdout => |res| {
                    if (res) |len| {
                        task.stdout.appendSlice(task.alloc, stdout_buffer[0..len]) catch {};
                    } else |_| {
                        select.cancelDiscard();
                        break;
                    }
                    select.concurrent(.stdout, std.Io.File.readStreaming, .{ stdout, io, &stdout_buffers }) catch {};
                },
                .stderr => |res| {
                    if (res) |len| {
                        task.stdout.appendSlice(task.alloc, stderr_buffer[0..len]) catch {};
                    } else |_| {
                        select.cancelDiscard();
                        break;
                    }
                    select.concurrent(.stderr, std.Io.File.readStreaming, .{ stderr, io, &stderr_buffers }) catch {};
                },
            }
        }
    }

    if (task.process.wait(io)) |term| {
        task.term = term;
    } else |_| {}
    task.progress_node.end();

    queue.putOne(io, task.id) catch |err| {
        std.log.err("Cannot add to queue: {}", .{err});
    };
}

pub fn main(init: std.process.Init) !void {
    const io = init.io;

    try utils.check_root(io);

    var args = init.minimal.args.iterate();
    const path = args.next();
    _ = path;

    const filter = if (args.next()) |bin| bin else "";

    var arena = std.heap.ArenaAllocator.init(init.gpa);
    defer arena.deinit();
    const alloc = arena.allocator();

    print("Building the test runner:\n", .{});
    try utils.run(io, &.{ "zig", "build", "test-runner" });

    const cwd = try std.Io.Dir.cwd().realPathFileAlloc(io, ".", alloc);
    const test_runner_path = try std.Io.Dir.cwd().realPathFileAlloc(io, "zig-out/bin/rvman-test-runner", alloc);
    var dir = try std.Io.Dir.cwd().openDir(io, try std.fs.path.join(alloc, &.{ "local", "share", "riscv-tests", "isa" }), .{ .iterate = true });
    defer dir.close(io);
    var dir_iter = dir.iterate();

    var tests: std.ArrayList(*Task) = .empty;

    print("\nRunning tests:\n", .{});
    const progress = std.Progress.start(io, .{});

    var queue_buffer: [1]u32 = undefined;
    var queue = std.Io.Queue(u32).init(&queue_buffer);

    while (try dir_iter.next(io)) |entry| {
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

        var stdio_type: std.process.SpawnOptions.StdIo = undefined;
        if (!try init.minimal.environ.contains(alloc, "SIMPLE")) {
            stdio_type = .pipe;
        } else {
            stdio_type = .close;
        }

        const progress_node = progress.start(entry.name, 0);

        task.* = .{
            .id = last_len + 1,
            .name = name,
            .alloc = init.gpa,
            .process = try std.process.spawn(io, .{ .argv = argv, .stdout = stdio_type, .stderr = stdio_type, .progress_node = progress_node }),
            .progress_node = progress_node,
            .stdout = .empty,
        };
        _ = try std.Thread.spawn(.{ .allocator = alloc }, handle_test, .{ io, &queue, task });
    }

    var finished: usize = 0;
    while (tests.items.len != 0) {
        _ = try queue.getOne(io);
        finished += 1;

        if (finished == tests.items.len) {
            break;
        }
        continue;
    }

    progress.end();

    var failed: usize = 0;
    var passed: usize = 0;

    for (tests.items) |task| {
        if (task.term) |term| {
            switch (term) {
                .exited => |code| {
                    if (code == 0) {
                        passed += 1;
                        print("\x1b[32mPass\x1B[0m {s}\n", .{task.name});
                        task.stdout.deinit(task.alloc);
                        continue;
                    }
                },
                else => {},
            }
        } else {
            print("\tNo\n", .{});
        }
        failed += 1;

        if (!try init.minimal.environ.contains(alloc, "SIMPLE")) {
            print("\x1B[91mFailed\x1B[0m {s}\n", .{task.name});
            print("{s}", .{task.stdout.items});
        }
        print("\x1B[91mFailed\x1B[0m {s}\n", .{task.name});
        task.stdout.deinit(task.alloc);
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
