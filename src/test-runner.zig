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

            var cpu = try CPU.init(gpa.allocator());
            defer cpu.deinit();

            const ELF = elf.build(CPU);

            var object = ELF.load(&cpu, program_path) catch |err| {
                print("Fail to load program: {s} Error: {}\n", .{ program_path, err });
                return;
            };
            defer object.deinit();

            const tohost_addr = object.sections.get(".tohost").?;

            const tohost_result: *u32 = @ptrFromInt(try cpu.map_to_memory(tohost_addr));
            const tohost_zero: *u32 = @ptrFromInt(try cpu.map_to_memory(tohost_addr + 0x4));

            while (cpu.harts[0].step(&cpu)) {
                if (tohost_zero.* != 0) {
                    @panic("This should be zero, .tohost invalid memory");
                }
                if (tohost_result.* & 1 != 1) {
                    continue;
                }
                const result = tohost_result.* >> 1;
                print("{}\n", .{result});
                if (result != 0) {
                    std.process.exit(1);
                } else {
                    std.process.exit(0);
                }
            } else |err| {
                print("Exited with: {}\n", .{err});
                return err;
            }
        },
        64 => {
            const CPU = riscv.buildCPU(.X64, 1);

            var cpu = try CPU.init(gpa.allocator());
            defer cpu.deinit();

            const ELF = elf.build(CPU);
            var object = ELF.load(&cpu, program_path) catch |err| {
                print("Fail to load program: {s} Error: {}\n", .{ program_path, err });
                return;
            };
            defer object.deinit();

            const tohost_addr = object.sections.get(".tohost").?;

            var memory: [8]u8 = undefined;
            var test_memory: [8]u8 = undefined;
            const riscv_asm = @import("riscv/asm.zig");
            const ASM = riscv_asm.build_asm(.X64);
            var instr: ASM = undefined;
            var old_values = std.mem.zeroes([4]u64);
            const tohost_result: *u32 = @ptrFromInt(try cpu.map_to_memory(tohost_addr));
            const tohost_zero: *u32 = @ptrFromInt(try cpu.map_to_memory(tohost_addr + 0x4));
            while (d: {
                _ = try cpu.vmemory_read(cpu.harts[0].pc, &memory);
                instr = try ASM.from_memory(&memory);
                print("{s}: 0x{x} ", .{ cpu.harts[0].mode.name(), cpu.harts[0].pc });
                try instr.write(std.io.getStdErr().writer().any());
                for (0..instr.used_grs().len) |i| {
                    old_values[i] = cpu.harts[0].g_regs[instr.used_grs()[i].to_u5()];
                }
                const len = try instr.to_memory(&test_memory);
                if (!std.mem.eql(u8, memory[0..instr.len()], test_memory[0..len])) {
                    std.mem.reverse(u8, memory[0..instr.len()]);
                    std.mem.reverse(u8, test_memory[0..len]);
                    print("Before: {b:0>8}\n", .{memory[0..instr.len()]});
                    print("After: {b:0>8}\n", .{test_memory[0..len]});
                    return error.LossyDissasambler;
                }
                break :d cpu.harts[0].step(&cpu);
            }) {
                for (0..instr.used_grs().len) |i| {
                    const reg = instr.used_grs()[i];
                    if (reg.to_u5() == 0) continue;
                    print("\tReg: {s} = 0x{x} = 0x{x}\n", .{ riscv.IntRegNames[reg.to_u5()], old_values[i], cpu.harts[0].g_regs[reg.to_u5()] });
                }
                if (tohost_zero.* != 0) {
                    @panic("This should be zero, .tohost invalid memory");
                }
                if (tohost_result.* & 1 != 1) {
                    continue;
                }
                const result = tohost_result.* >> 1;
                print("{}\n", .{result});
                if (result != 0) {
                    std.process.exit(1);
                } else {
                    std.process.exit(0);
                }
            } else |err| {
                print("Exited with: {}\n", .{err});
                return err;
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
