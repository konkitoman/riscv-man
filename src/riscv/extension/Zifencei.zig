const std = @import("std");
const base = @import("../base.zig");

const rearrange = base.rearrange;

const debug = std.debug;

const Arch = base.Arch;
const IFX32 = base.InstrFormatX32;

const Instruction = base.Instruction;

pub fn FENCE_I(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0001111) return false; // MISC-MEM opcode
            if (x32_instr.f.func3 != 0b001) return false; // FENCE.I func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 4);

            hart_data.I.pc += 4;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn buildInstrs(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) [1]Instruction(ARCH, DataEEI, DataHart) {
    return .{
        FENCE_I(ARCH, DataEEI, DataHart).instr(),
    };
}
