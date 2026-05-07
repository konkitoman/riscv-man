const std = @import("std");
const path = std.fs.path;
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const c = @cImport({
    @cInclude("stdlib.h");
});

pub fn check_root(io: std.Io) !void {
    const cwd = std.Io.Dir.cwd();
    var path_buff = std.mem.zeroes([std.fs.max_path_bytes]u8);
    const p = try cwd.realPathFile(io, ".", &path_buff);
    var path_iter = std.mem.splitBackwardsScalar(u8, path_buff[0..p], std.fs.path.sep);
    if (path_iter.next()) |segment| {
        if (std.mem.eql(u8, segment, "scripts")) {
            print("You should run this script from the project root!\n", .{});
            std.process.exit(1);
        }
    }
}

pub fn exists(io: std.Io, name: []const u8) bool {
    if (std.Io.Dir.cwd().statFile(io, name, .{})) |_| {
        return true;
    } else |_| {
        return false;
    }
}

pub fn run(io: std.Io, argv: []const []const u8) !void {
    var process = try std.process.spawn(io, .{
        .argv = argv,
        .stdin = .inherit,
        .stdout = .inherit,
        .stderr = .inherit,
    });
    const process_term = try process.wait(io);
    switch (process_term) {
        .exited => |code| {
            if (code != 0) {
                print("Exit code: {d}\n", .{code});
                return error.Fail;
            }
        },
        else => {
            return error.Dead;
        },
    }
}

pub const GitOptions = struct {
    recursive: bool = false,
};

pub fn git_clone(gpa: std.mem.Allocator, repo: []const u8, out: ?[]const u8, options: GitOptions) !void {
    var args = std.ArrayList([]const u8){};
    defer args.deinit(gpa);

    try args.append(gpa, "git");
    try args.append(gpa, "clone");

    if (options.recursive) {
        try args.append(gpa, "--recursive");
    }

    try args.append(gpa, repo);
    if (out) |out_name| {
        try args.append(gpa, out_name);
    }

    if (run(gpa, args.items)) |_| {
        return;
    } else |_| {
        return error.CannotGitClone;
    }
}

pub fn install(allocator: Allocator, from: [][]const u8, to: [][]const u8) !void {
    const from_p = try path.join(allocator, from);
    defer allocator.free(from_p);
    const to_p = try path.join(allocator, to);
    defer allocator.free(to_p);

    var dir = try std.fs.openDirAbsolute(from_p, .{ .iterate = true });
    defer dir.close();
    var iterator = dir.iterate();
    while (try iterator.next()) |entry| {
        const p = try path.join(allocator, &.{ to_p, entry.name });
        defer allocator.free(p);

        try dir.symLink(entry.name, p, .{ .is_directory = entry.kind == .directory });
    }
}

fn prepare_gnu_toolchain(allocator: Allocator) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    if (!exists("riscv-gnu-toolchain")) {
        try git_clone(alloc, "https://github.com/riscv-collab/riscv-gnu-toolchain.git", "riscv-gnu-toolchain", .{});
    }

    if (!exists("local")) {
        try std.fs.cwd().makeDir("local");
    }
}

fn install_gnu_toolchain(allocator: Allocator) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const cwd = try std.fs.cwd().realpathAlloc(alloc, ".");
    const toolchain_path = try path.join(alloc, &.{ cwd, "local" });

    try std.process.changeCurDir("riscv-gnu-toolchain");

    try run(alloc, &.{ "./configure", "--prefix", toolchain_path });
    try run(alloc, &.{ "make", "-j32" });

    try std.process.changeCurDir("..");
}

fn setenv(name: []const u8, value: []const u8) !void {
    if (c.setenv(name.ptr, value.ptr, 1) != 0) {
        return error.CannotSetEnv;
    }
}

/// # Needs libc
/// This will download, build and add to path the gnu toolchain
pub fn setup_gnu_toolchain(allocator: Allocator) !void {
    try check_root();

    if (exists("riscv-gnu-toolchain") or exists("official-riscv-tests") or exists("local")) {
        print("This is not clean!\n", .{});
    }

    try prepare_gnu_toolchain(allocator);
    try install_gnu_toolchain(allocator);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const cwd = try std.fs.cwd().realpathAlloc(alloc, ".");
    const toolchain_path = try path.join(alloc, &.{ cwd, "local" });

    const LD_LIBRARY_PATH = std.process.getEnvVarOwned(alloc, "LD_LIBRARY_PATH") catch "";
    const NEW_LD_LIBRARY_PATH = try std.fmt.allocPrint(alloc, "{s}:{s}\x00", .{ LD_LIBRARY_PATH, try path.join(alloc, &.{ toolchain_path, "lib" }) });
    try setenv("LD_LIBRARY_PATH", NEW_LD_LIBRARY_PATH);

    const PATH = try std.process.getEnvVarOwned(alloc, "PATH");
    const NEW_PATH = try std.fmt.allocPrint(alloc, "{s}:{s}\x00", .{ PATH, try path.join(alloc, &.{ toolchain_path, "bin" }) });
    try setenv("PATH", NEW_PATH);
}
