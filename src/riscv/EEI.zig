const std = @import("std");
const base = @import("base.zig");
const debug = std.debug;

pub fn Instruction(comptime ARCH: base.Arch, comptime DataCPU: type, comptime DataHart: type) type {
    _ = ARCH;

    return struct {
        check: *const fn (instr_data: []const u8) bool,
        execute: *const fn (cpu_data: *DataCPU, hart_data: *DataHart, instr_data: []const u8) void,
    };
}

pub fn buildEEI(comptime ARCH: base.Arch, comptime harts: usize, comptime DataEEI: type, comptime DataHart: type, comptime instructions: []const Instruction(ARCH, DataEEI, DataHart)) type {
    return struct {
        pub const Hart = struct {
            data: DataHart,

            fn step(self: *@This(), eei: *EEI) void {
                var instr_data: [4]u8 = undefined;

                if (!self.data.read(eei, self.data.I.pc, &instr_data)) return;

                var run_with: ?usize = null;

                for (0..instructions.len) |i| {
                    if (instructions[i].check(instr_data)) {
                        if (run_with) |_| {
                            @panic("Instruction collision");
                        }
                        run_with = i;
                    }
                }

                if (run_with) |i| {
                    instructions[i].execute(&eei.data, &self.data, instr_data);
                } else {
                    @panic("Unknown instruction");
                }
            }
        };

        data: DataEEI,
        harts: [harts]Hart,

        const EEI = @This();
    };
}
