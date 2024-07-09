const std = @import("std");
const riscv = @import("src/riscv.zig");
const elf = @import("src/elf.zig");

const print = std.debug.print;

pub fn main() !void {
    var args = std.process.args();
    const path = args.next();
    _ = path;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var cpu_meta = if (args.next()) |bin| bin else {
        print("cpu meta is required!\n", .{});
        return;
    };

    var arch: u8 = undefined;
    {
        var o_arch: ?u8 = null;

        while (cpu_meta.len > 0) {
            switch (cpu_meta[0]) {
                'R' => {
                    if (cpu_meta.len < 4) {
                        return error.CpuMetaInvalidR;
                    }
                    if (cpu_meta[1] == 'V') {
                        if (std.mem.eql(u8, cpu_meta[2..4], "32")) {
                            o_arch = 32;
                            cpu_meta = cpu_meta[4..];
                            continue;
                        } else if (std.mem.eql(u8, cpu_meta[2..4], "64")) {
                            o_arch = 64;
                            cpu_meta = cpu_meta[4..];
                            continue;
                        }
                    }
                },
                else => {
                    return error.CPUMetaInvalid;
                },
            }
        }

        arch = o_arch orelse {
            return error.CPUMetaNoArch;
        };
    }

    const program_path = if (args.next()) |bin| bin else {
        print("program path is required!\n", .{});
        return;
    };

    switch (arch) {
        32 => {
            const CPU =
                riscv.buildCPU(u32, 1);
            const eei = build(CPU);

            var cpu = CPU.init(gpa.allocator(), &.{}, .{ .ecall = eei.ecall });
            defer cpu.deinit();

            elf.load(CPU, &cpu, program_path) catch |err| {
                print("Fail to load program: {s} Error: {}\n", .{ program_path, err });
                return;
            };

            while (cpu.step()) {} else |err| {
                print("Exited with: {}\n", .{err});
            }
        },
        64 => {
            const CPU = riscv.buildCPU(u64, 1);
            const eei = build(CPU);

            var cpu = CPU.init(gpa.allocator(), &.{}, .{ .ecall = eei.ecall });
            defer cpu.deinit();

            elf.load(CPU, &cpu, program_path) catch |err| {
                print("Fail to load program: {s} Error: {}\n", .{ program_path, err });
                return;
            };

            while (cpu.step()) {} else |err| {
                print("Exited with: {}\n", .{err});
            }
        },
        else => {
            unreachable;
        },
    }
}

pub fn build(comptime CPU: type) type {
    return struct {
        fn ecall(hart: *CPU.Hart, cpu: *CPU) void {
            _ = cpu;
            if (hart.g_regs[riscv.GeneralReg.A7.to_u5()] == 93) {
                const a0 = hart.g_regs[riscv.GeneralReg.A0.to_u5()];
                if (a0 == 0) {
                    print("Pass\n", .{});
                    std.process.exit(0);
                } else {
                    print("Fail: {d}\n", .{a0 >> 1});
                    std.process.exit(1);
                }
            }
        }
    };
}
