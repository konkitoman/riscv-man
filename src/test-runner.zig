const std = @import("std");
const riscv = @import("riscv/cpu.zig");
const elf = @import("elf.zig");

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
                riscv.buildCPU(.X32, 1);
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
            const CPU = riscv.buildCPU(.X64, 1);
            const eei = build(CPU);

            var cpu = CPU.init(gpa.allocator(), &.{}, .{ .ecall = eei.ecall });
            defer cpu.deinit();

            elf.load(CPU, &cpu, program_path) catch |err| {
                print("Fail to load program: {s} Error: {}\n", .{ program_path, err });
                return;
            };

            var memory: [8]u8 = undefined;
            var test_memory: [8]u8 = undefined;
            const ASM = @import("riscv/asm.zig").build_asm(.X64);
            var instr: ASM = undefined;
            while (d: {
                _ = try cpu.vmemory_read(cpu.harts[0].pc, &memory);
                instr = try ASM.from_memory(&memory);
                print("0x{x} ", .{cpu.harts[0].pc});
                try instr.write(std.io.getStdErr().writer().any());
                for (instr.used_grs()) |reg| {
                    if (reg.to_u5() == 0) continue;
                    print("\tOld Reg: {s} = 0x{x}\n", .{ riscv.IntRegNames[reg.to_u5()], cpu.harts[0].g_regs[reg.to_u5()] });
                }
                const len = try instr.to_memory(&test_memory);
                if (!std.mem.eql(u8, memory[0..instr.len()], test_memory[0..len])) {
                    std.mem.reverse(u8, memory[0..instr.len()]);
                    std.mem.reverse(u8, test_memory[0..len]);
                    print("Before: {b:0>8}\n", .{memory[0..instr.len()]});
                    print("After: {b:0>8}\n", .{test_memory[0..len]});
                    return error.LossyDissasambler;
                }
                break :d cpu.step();
            }) {
                for (instr.used_grs()) |reg| {
                    if (reg.to_u5() == 0) continue;
                    print("\tNew Reg: {s} = 0x{x}\n", .{ riscv.IntRegNames[reg.to_u5()], cpu.harts[0].g_regs[reg.to_u5()] });
                }
            } else |err| {
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
            if (hart.g_regs[riscv.IntReg.A7.to_u5()] == 93) {
                const a0 = hart.g_regs[riscv.IntReg.A0.to_u5()];
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
