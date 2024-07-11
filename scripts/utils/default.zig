const std = @import("std");
const path = std.fs.path;
const print = std.debug.print;
const Allocator = std.mem.Allocator;

pub fn check_root() !void {
    const cwd = std.fs.cwd();
    var path_buff = std.mem.zeroes([std.fs.MAX_PATH_BYTES]u8);
    const p = try cwd.realpath(".", &path_buff);
    var path_iter = std.mem.splitBackwardsScalar(u8, p, std.fs.path.sep);
    if (path_iter.next()) |segment| {
        if (std.mem.eql(u8, segment, "scripts")) {
            print("You should run this script from the project root!\n", .{});
            std.process.exit(1);
        }
    }
}

pub fn exists(name: []const u8) bool {
    if (std.fs.cwd().statFile(name)) |_| {
        return true;
    } else |_| {
        return false;
    }
}

pub fn run(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var process = std.process.Child.init(argv, allocator);
    process.stdin_behavior = .Inherit;
    process.stdout_behavior = .Inherit;
    process.stderr_behavior = .Inherit;
    const process_term = try process.spawnAndWait();
    switch (process_term) {
        .Exited => |code| {
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

pub fn git_clone(allocator: std.mem.Allocator, repo: []const u8, out: ?[]const u8, options: GitOptions) !void {
    var args = std.ArrayList([]u8).init(allocator);
    defer args.deinit();

    args.append("git");
    args.append("clone");

    if (options.recursive) {
        args.append("--recursive");
    }

    args.append(repo);
    if (out) |out_name| {
        args.append(out_name);
    }

    if (run(allocator, args.items)) |_| {
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
