const std = @import("std");
const path = std.fs.path;
const print = std.debug.print;
const Allocator = std.mem.Allocator;

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

pub fn run(io: std.Io, environ: ?*std.process.Environ.Map, argv: []const []const u8) !void {
    var process = try std.process.spawn(io, .{
        .argv = argv,
        .stdin = .inherit,
        .stdout = .inherit,
        .stderr = .inherit,
        .environ_map = environ,
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

pub fn git_clone(gpa: std.mem.Allocator, io: std.Io, repo: []const u8, out: ?[]const u8, options: GitOptions) !void {
    var args: std.ArrayList([]const u8) = .empty;
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

    if (run(io, null, args.items)) |_| {
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

fn prepare_gnu_toolchain(allocator: Allocator, io: std.Io) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    if (!exists(io, "riscv-gnu-toolchain")) {
        try git_clone(alloc, io, "https://github.com/riscv-collab/riscv-gnu-toolchain.git", "riscv-gnu-toolchain", .{});
    }

    if (!exists(io, "local")) {
        try std.Io.Dir.cwd().createDirPath(io, "local");
    }
}

fn install_gnu_toolchain(allocator: Allocator, io: std.Io) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const cwd = try std.Io.Dir.cwd().realPathFileAlloc(io, ".", alloc);
    const toolchain_path = try path.join(alloc, &.{ cwd, "local" });

    try std.process.setCurrentPath(io, "riscv-gnu-toolchain");

    try run(io, null, &.{ "./configure", "--prefix", toolchain_path });
    try run(io, null, &.{ "make", "-j32" });

    try std.process.setCurrentPath(io, "..");
}

/// # Needs libc
/// This will download, build and add to path the gnu toolchain
pub fn setup_gnu_toolchain(gpa: Allocator, io: std.Io, environ: *std.process.Environ.Map) !void {
    try check_root(io);

    if (exists(io, "riscv-gnu-toolchain") or exists(io, "official-riscv-tests") or exists(io, "local")) {
        print("This is not clean!\n", .{});
    }

    try prepare_gnu_toolchain(gpa, io);
    try install_gnu_toolchain(gpa, io);

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const alloc = arena.allocator();

    const cwd = try std.Io.Dir.cwd().realPathFileAlloc(io, ".", alloc);
    const toolchain_path = try path.join(alloc, &.{ cwd, "local" });

    const library_path = environ.get("LD_LIBRARY_PATH") orelse "";
    const new_library_path = try std.fmt.allocPrint(alloc, "{s}:{s}\x00", .{ library_path, try path.join(alloc, &.{ toolchain_path, "lib" }) });
    try environ.put("LD_LIBRARY_PATH", new_library_path);

    const new_path = try std.fmt.allocPrint(alloc, "{s}:{s}\x00", .{ environ.get("PATH") orelse "", try path.join(alloc, &.{ toolchain_path, "bin" }) });
    try environ.put("PATH", new_path);
}
