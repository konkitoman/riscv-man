const std = @import("std");
const base = @import("../base.zig");
const rearrange = base.rearrange;
const debug = std.debug;

const Arch = base.Arch;

const Instruction = base.Instruction;

pub fn MUL(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;
            const data = std.mem.readInt(u32, @ptrCast(instr_data), .little);

            if (data & 0b1111111 != 0b0110011 // OP
            or (data >> 12) & 0b111 != 0b000 // func3 MUL
            or data >> 25 != 0b0000001) return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 4);
            const data = std.mem.readInt(u32, @ptrCast(instr_data), .little);

            const rd = base.IntReg.from_u5(@truncate(data >> 7));
            const rs1 = base.IntReg.from_u5(@truncate(data >> 15));
            const rs2 = base.IntReg.from_u5(@truncate(data >> 20));

            if (rd != .ZERO) {
                hart_data.I.regs[rd.to_u5()] = hart_data.I.regs[rs2.to_u5()] *% hart_data.I.regs[rs1.to_u5()];
            }
            hart_data.I.pc +%= 4;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "MUL",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn DIV(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;
            const data = std.mem.readInt(u32, @ptrCast(instr_data), .little);

            if (data & 0b1111111 != 0b0110011 // OP
            or (data >> 12) & 0b111 != 0b100 // func3 DIV
            or data >> 25 != 0b0000001) return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 4);
            const data = std.mem.readInt(u32, @ptrCast(instr_data), .little);

            const rd = base.IntReg.from_u5(@truncate(data >> 7));
            const rs1 = base.IntReg.from_u5(@truncate(data >> 15));
            const rs2 = base.IntReg.from_u5(@truncate(data >> 20));

            if (rd != .ZERO) {
                if (hart_data.I.regs[rs2.to_u5()] == 0) {
                    hart_data.I.regs[rd.to_u5()] = std.math.maxInt(ARCH.uarch());
                } else {
                    hart_data.I.regs[rd.to_u5()] = @bitCast(std.math.divTrunc(ARCH.iarch(), @as(ARCH.iarch(), @bitCast(hart_data.I.regs[rs1.to_u5()])), @as(ARCH.iarch(), @bitCast(hart_data.I.regs[rs2.to_u5()]))) catch @as(ARCH.iarch(), @bitCast(hart_data.I.regs[rs1.to_u5()])));
                }
            }
            hart_data.I.pc +%= 4;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "DIV",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn buildInstrs(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) [2]Instruction(ARCH, DataEEI, DataHart) {
    return .{
        MUL(ARCH, DataEEI, DataHart).instr(),
        //MULH(ARCH, DataEEI, DataHart).instr(),
        //MULHSU(ARCH, DataEEI, DataHart).instr(),
        //MULHU(ARCH, DataEEI, DataHart).instr(),
        DIV(ARCH, DataEEI, DataHart).instr(),
        //DIVU(ARCH, DataEEI, DataHart).instr(),
        //REM(ARCH, DataEEI, DataHart).instr(),
        //REMU(ARCH, DataEEI, DataHart).instr(),
        //MULW(ARCH, DataEEI, DataHart).instr(),
        //DIVW(ARCH, DataEEI, DataHart).instr(),
        //DIVUW(ARCH, DataEEI, DataHart).instr(),
        //REMW(ARCH, DataEEI, DataHart).instr(),
        //REMUW(ARCH, DataEEI, DataHart).instr(),
    };
}
