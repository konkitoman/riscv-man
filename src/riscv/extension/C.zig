const std = @import("std");
const base = @import("../base.zig");
const rearrange = base.rearrange;
const debug = std.debug;

const Arch = base.Arch;

const Instruction = base.Instruction;

pub fn buildDataHart(comptime ARCH: base.Arch) type {
    _ = ARCH;
    return struct {};
}

pub fn C_LWSP(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b10 // OP C2
            or data >> 13 != 0b010 // func3 C.LWSP
            or (data >> 7) & 0b11111 == 0 // rd != 0
            ) return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const imm = rearrange(u16, u8, data, &.{ .{ 2, 2, 6 }, .{ 4, 3, 2 }, .{ 12, 1, 5 } });
            const offset: ARCH.uarch() = hart_data.I.regs[base.IntReg.SP.to_u5()] +% imm;
            const rd: u5 = @truncate((data >> 7));
            var buffer: [4]u8 = undefined;

            if (!hart_data.read(eei_data, offset, &buffer)) return;
            hart_data.I.regs[rd] = @bitCast(@as(ARCH.iarch(), std.mem.readInt(i32, &buffer, .little)));
            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_LWSP",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_LDSP(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b10 // OP C2
            or data >> 13 != 0b011 // func3 C.LDSP
            or (data >> 7) & 0b11111 == 0 // rd != 0
            ) return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            if (ARCH == .X32) unreachable;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const imm = rearrange(u16, u8, data, &.{ .{ 2, 3, 6 }, .{ 5, 2, 3 }, .{ 12, 1, 5 } });
            const offset: ARCH.uarch() = hart_data.I.regs[base.IntReg.SP.to_u5()] +% imm;
            const rd: u5 = @truncate((data >> 7));
            var buffer: [8]u8 = undefined;

            if (!hart_data.read(eei_data, offset, &buffer)) return;
            hart_data.I.regs[rd] = @bitCast(@as(ARCH.iarch(), std.mem.readInt(i64, &buffer, .little)));
            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_LDSP",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_SWSP(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b10 // OP C2
            or data >> 13 != 0b110 // func3 C.SWSP
            ) return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const imm = rearrange(u16, u8, data, &.{ .{ 7, 2, 6 }, .{ 9, 4, 2 } });
            const rs2: u5 = @truncate(data >> 2);
            const offset: ARCH.uarch() = hart_data.I.regs[base.IntReg.SP.to_u5()] +% imm;
            var buffer: [4]u8 = undefined;
            std.mem.writeInt(u32, &buffer, @truncate(hart_data.I.regs[rs2]), .little);

            if (!hart_data.write(eei_data, offset, &buffer)) return;
            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_SWSP",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_SDSP(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b10 // OP C2
            or data >> 13 != 0b111 // func3 C.SDSP
            ) return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            if (ARCH == .X32) unreachable;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const imm = rearrange(u16, u8, data, &.{ .{ 7, 3, 6 }, .{ 10, 3, 3 } });
            const rs2: u5 = @truncate(data >> 2);
            const offset: ARCH.uarch() = hart_data.I.regs[base.IntReg.SP.to_u5()] +% imm;
            var buffer: [8]u8 = undefined;
            std.mem.writeInt(u64, &buffer, @truncate(hart_data.I.regs[rs2]), .little);

            if (!hart_data.write(eei_data, offset, &buffer)) return;
            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_SDSP",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_LW(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b00 // OP C0
            or data >> 13 != 0b010 // func3 C.LW
            ) return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const imm = rearrange(u16, u7, data, &.{ .{ 5, 1, 6 }, .{ 6, 1, 2 }, .{ 10, 3, 3 } });
            const rd: u5 = base.PopularIntReg.from_u3(@truncate((data >> 2))).to_reg().to_u5();
            const rs1: u5 = base.PopularIntReg.from_u3(@truncate((data >> 7))).to_reg().to_u5();
            const offset: ARCH.uarch() = hart_data.I.regs[rs1] +% imm;
            var buffer: [4]u8 = undefined;

            if (!hart_data.read(eei_data, offset, &buffer)) return;
            hart_data.I.regs[rd] = @bitCast(@as(ARCH.iarch(), std.mem.readInt(i32, &buffer, .little)));
            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_LW",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_LD(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;

            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b00 // OP C0
            or data >> 13 != 0b011 // func3 C.LD
            ) return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            if (ARCH == .X32) unreachable;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const imm = rearrange(u16, u7, data, &.{ .{ 5, 1, 6 }, .{ 10, 3, 3 } });
            const rd: u5 = base.PopularIntReg.from_u3(@truncate((data >> 2))).to_reg().to_u5();
            const rs1: u5 = base.PopularIntReg.from_u3(@truncate((data >> 7))).to_reg().to_u5();
            const offset: ARCH.uarch() = hart_data.I.regs[rs1] +% imm;
            var buffer: [8]u8 = undefined;

            if (!hart_data.read(eei_data, offset, &buffer)) return;
            hart_data.I.regs[rd] = @bitCast(@as(ARCH.iarch(), std.mem.readInt(i64, &buffer, .little)));
            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_LD",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_SW(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b00 // OP C0
            or data >> 13 != 0b110 // func3 C.SW
            ) return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const imm = rearrange(u16, u7, data, &.{ .{ 5, 1, 6 }, .{ 6, 1, 2 }, .{ 10, 3, 3 } });
            const rs2: u5 = base.PopularIntReg.from_u3(@truncate((data >> 2))).to_reg().to_u5();
            const rs1: u5 = base.PopularIntReg.from_u3(@truncate((data >> 7))).to_reg().to_u5();
            const offset: ARCH.uarch() = hart_data.I.regs[rs1] +% imm;
            var buffer: [4]u8 = undefined;
            std.mem.writeInt(u32, &buffer, @truncate(hart_data.I.regs[rs2]), .little);

            if (!hart_data.write(eei_data, offset, &buffer)) return;
            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_SW",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_SD(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;

            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b00 // OP C0
            or data >> 13 != 0b111 // func3 C.SD
            ) return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            if (ARCH == .X32) unreachable;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const imm = rearrange(u16, u7, data, &.{ .{ 5, 1, 6 }, .{ 10, 3, 3 } });
            const rs2: u5 = base.PopularIntReg.from_u3(@truncate((data >> 2))).to_reg().to_u5();
            const rs1: u5 = base.PopularIntReg.from_u3(@truncate((data >> 7))).to_reg().to_u5();
            const offset: ARCH.uarch() = hart_data.I.regs[rs1] +% imm;
            var buffer: [8]u8 = undefined;
            std.mem.writeInt(u64, &buffer, @truncate(hart_data.I.regs[rs2]), .little);

            if (!hart_data.write(eei_data, offset, &buffer)) return;
            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_SW",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_J(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const IMM = .{ .{ 2, 1, 5 }, .{ 3, 3, 1 }, .{ 6, 1, 7 }, .{ 7, 1, 6 }, .{ 8, 1, 10 }, .{ 9, 2, 8 }, .{ 11, 1, 4 }, .{ 12, 1, 11 } };

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b101 // func3 C.J
            )
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const imm = rearrange(u16, u12, data, &IMM);

            hart_data.I.pc +%= @as(ARCH.uarch(), @bitCast(@as(ARCH.iarch(), @as(i12, @bitCast(imm)))));
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_J",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_JAL(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const IMM = .{ .{ 2, 1, 5 }, .{ 3, 3, 1 }, .{ 6, 1, 7 }, .{ 7, 1, 6 }, .{ 8, 1, 10 }, .{ 9, 2, 8 }, .{ 11, 1, 4 }, .{ 12, 1, 11 } };

    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH != .X32) return false;
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b001 // func3 C.JAL
            )
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            if (ARCH != .X32) unreachable;
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const imm = rearrange(u16, u12, data, &IMM);

            hart_data.I.regs[base.IntReg.RA.to_u5()] = hart_data.I.pc +% 2;
            hart_data.I.pc +%= @as(ARCH.uarch(), @bitCast(@as(ARCH.iarch(), @as(i12, @bitCast(imm)))));
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_JAL",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_JR(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b10 // OP C2
            or (data >> 2) & 0b11111 != 0 // rs2 zero
            or (data >> 7) & 0b11111 == 0 // rs1 not zero
            or data >> 12 != 0b1000 // func3 C.JR
            )
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const rs1: u5 = @truncate(data >> 7);

            hart_data.I.pc = hart_data.I.regs[rs1];
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_JR",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_JALR(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b10 // OP C2
            or (data >> 2) & 0b11111 != 0 // rs2 zero
            or (data >> 7) & 0b11111 == 0 // rs1 not zero
            or data >> 12 != 0b1001 // func3 C.JALR
            )
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const rs1: u5 = @truncate(data >> 7);

            hart_data.I.regs[base.IntReg.RA.to_u5()] = hart_data.I.pc + 2;
            hart_data.I.pc = hart_data.I.regs[rs1];
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_JR",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_BEQZ(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const IMM = .{ .{ 2, 1, 5 }, .{ 3, 2, 1 }, .{ 5, 2, 6 }, .{ 10, 2, 3 }, .{ 12, 1, 8 } };

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b110 // func3 C.BEQZ
            )
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const imm = rearrange(u16, u9, data, &IMM);
            const rs1 = base.PopularIntReg.from_u3(@truncate(data >> 7)).to_reg().to_u5();
            const offset: ARCH.uarch() = @bitCast(@as(ARCH.iarch(), @as(i9, @bitCast(imm))));

            if (hart_data.I.regs[rs1] == 0) {
                hart_data.I.pc +%= offset;
            } else {
                hart_data.I.pc +%= 2;
            }
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_BEQZ",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_BNEZ(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const IMM = .{ .{ 2, 1, 5 }, .{ 3, 2, 1 }, .{ 5, 2, 6 }, .{ 10, 2, 3 }, .{ 12, 1, 8 } };

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b111 // func3 C.BNEZ
            )
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const imm = rearrange(u16, u9, data, &IMM);
            const rs1 = base.PopularIntReg.from_u3(@truncate(data >> 7)).to_reg().to_u5();
            const offset: ARCH.uarch() = @bitCast(@as(ARCH.iarch(), @as(i9, @bitCast(imm))));

            if (hart_data.I.regs[rs1] != 0) {
                hart_data.I.pc +%= offset;
            } else {
                hart_data.I.pc +%= 2;
            }
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_BNEZ",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_LI(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const IMM = .{ .{ 2, 5, 0 }, .{ 12, 1, 5 } };

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b010 // func3 C.LI
            or (data >> 7) & 0b11111 == 0 // rd should not be zero
            )
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const rd: u5 = @truncate(data >> 7);
            const imm = rearrange(u16, u6, data, &IMM);

            hart_data.I.regs[rd] = @as(ARCH.uarch(), @bitCast(@as(ARCH.iarch(), @as(i6, @bitCast(imm)))));

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_LI",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_LUI(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const IMM = .{ .{ 2, 5, 12 }, .{ 12, 1, 17 } };

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b011 // func3 C.LUI
            or (data >> 7) & 0b11111 == 0 // rd should not be zero
            or (data >> 7) & 0b11111 == 2 // rd should not be SP
            or rearrange(u16, u18, data, &IMM) == 0 // immediate should not be zero
            )
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const rd: u5 = @truncate(data >> 7);
            const imm = rearrange(u16, u18, data, &IMM);

            hart_data.I.regs[rd] = @as(ARCH.uarch(), @bitCast(@as(ARCH.iarch(), @as(i18, @bitCast(imm)))));

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_LUI",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_ADDI(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const IMM = .{ .{ 2, 5, 0 }, .{ 12, 1, 5 } };

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b000 // func3 C.ADDI
            or (data >> 7) & 0b11111 == 0 // rd/rs1 should not be zero
            or rearrange(u16, u6, data, &IMM) == 0) // non zero immediate
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const rd: u5 = @truncate(data >> 7);
            const imm = rearrange(u16, u6, data, &IMM);

            hart_data.I.regs[rd] +%= @as(ARCH.uarch(), @bitCast(@as(ARCH.iarch(), @as(i6, @bitCast(imm)))));

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_ADDI",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_ADDIW(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const IMM = .{ .{ 2, 5, 0 }, .{ 12, 1, 5 } };

    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;

            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b001 // func3 C.ADDIW
            or (data >> 7) & 0b11111 == 0) // rd/rs1 should not be zero
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            if (ARCH == .X32) unreachable;
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const rd: u5 = @truncate(data >> 7);
            const imm = rearrange(u16, u6, data, &IMM);

            hart_data.I.regs[rd] = @bitCast(@as(ARCH.iarch(), @as(i32, @bitCast(@as(u32, @truncate(hart_data.I.regs[rd])) +% @as(u32, @bitCast(@as(i32, @as(i6, @bitCast(imm)))))))));

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_ADDIW",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_ADDI16SPN(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const IMM = .{ .{ 2, 1, 5 }, .{ 3, 2, 7 }, .{ 5, 1, 6 }, .{ 6, 1, 4 }, .{ 12, 1, 9 } };

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b011 // func3 C.ADDI16SPN
            or (data >> 7) & 0b11111 != base.IntReg.SP.to_u5() //
            or rearrange(u16, u10, data, &IMM) == 0) // non zero immediate
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const imm = rearrange(u16, u10, data, &IMM);

            hart_data.I.regs[base.IntReg.SP.to_u5()] +%= @as(ARCH.uarch(), @bitCast(@as(ARCH.iarch(), @as(i10, @bitCast(imm)))));

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_ADDI16SPN",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_ADDI4SPN(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b00 // OP C0
            or data >> 13 != 0b000 // func3 C.ADDI4SPN
            or (data >> 5) & std.math.maxInt(u12) == 0) return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            const imm = rearrange(u16, u12, data, &.{ .{ 5, 1, 3 }, .{ 6, 1, 2 }, .{ 7, 4, 6 }, .{ 11, 2, 4 } });
            const rd = base.PopularIntReg.from_u3(@truncate(data >> 2));

            hart_data.I.regs[rd.to_reg().to_u5()] = hart_data.I.regs[base.IntReg.SP.to_u5()] +% imm;

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_ADDI4SPN",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_SLLI(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const IMM = .{ .{ 2, 5, 0 }, .{ 12, 1, 5 } };

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b10 // OP C2
            or data >> 13 != 0b000 // func3 C.SLLI
            or (data >> 7 & 0b11111) == 0 //
            or (ARCH == .X32 and data & 1 << 12 == 1)) // shamt[5] needs to be zero in X32
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);
            const shamt: u6 = rearrange(u16, u6, data, &IMM);
            const rd: u5 = @truncate(data >> 7);

            hart_data.I.regs[rd] <<= @truncate(shamt);

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_SLLI",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_SRLI(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const IMM = .{ .{ 2, 5, 0 }, .{ 12, 1, 5 } };

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b100 // func3 C.SRLI
            or (data >> 10) & 0b11 != 0b00 //
            or (ARCH == .X32 and data & 1 << 12 == 1)) // shamt[5] needs to be zero in X32
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);
            const shamt: u6 = rearrange(u16, u6, data, &IMM);
            const rd: u5 = base.PopularIntReg.from_u3(@truncate(data >> 7)).to_reg().to_u5();

            hart_data.I.regs[rd] >>= @truncate(shamt);

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_SRLI",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_SRAI(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const IMM = .{ .{ 2, 5, 0 }, .{ 12, 1, 5 } };

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b100 // func3 C.SRAI
            or (data >> 10) & 0b11 != 0b01 or (ARCH == .X32 and data & 1 << 12 == 1)) // shamt[5] needs to be zero in X32
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);
            const shamt: u6 = rearrange(u16, u6, data, &IMM);
            const rd: u5 = base.PopularIntReg.from_u3(@truncate(data >> 7)).to_reg().to_u5();

            const sign_bit: ARCH.uarch() = comptime 1 << (ARCH.bytes() - 1);
            const sign: bool = hart_data.I.regs[rd] & sign_bit == sign_bit;
            hart_data.I.regs[rd] = hart_data.I.regs[rd] >> @truncate(shamt);

            if (sign) {
                const MAX: ARCH.uarch() = std.math.maxInt(ARCH.uarch());
                hart_data.I.regs[rd] |= MAX ^ (MAX >> @truncate(shamt));
            }

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_SRAI",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_ANDI(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const IMM = .{ .{ 2, 5, 0 }, .{ 12, 1, 5 } };

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b100 // func3 C.ANDI
            or (data >> 10) & 0b11 != 0b10)
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);
            const imm: u6 = rearrange(u16, u6, data, &IMM);
            const rd: u5 = base.PopularIntReg.from_u3(@truncate(data >> 7)).to_reg().to_u5();

            hart_data.I.regs[rd] &= @bitCast(@as(ARCH.iarch(), @as(i6, @bitCast(imm))));

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_ANDI",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_SUB(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b100 // func3 C.SUB
            or (data >> 10) & 0b11 != 0b11 //
            or (data >> 12 & 1 != 0) //
            or (data >> 5 & 0b11) != 0b00)
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);
            const rd: u5 = base.PopularIntReg.from_u3(@truncate(data >> 7)).to_reg().to_u5();
            const rs1: u5 = base.PopularIntReg.from_u3(@truncate(data >> 2)).to_reg().to_u5();

            hart_data.I.regs[rd] -%= hart_data.I.regs[rs1];

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_SUB",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_XOR(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b100 // func3 C.XOR
            or (data >> 10) & 0b11 != 0b11 //
            or (data >> 12 & 1 != 0) //
            or (data >> 5 & 0b11) != 0b01)
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);
            const rd: u5 = base.PopularIntReg.from_u3(@truncate(data >> 7)).to_reg().to_u5();
            const rs1: u5 = base.PopularIntReg.from_u3(@truncate(data >> 2)).to_reg().to_u5();

            hart_data.I.regs[rd] ^= hart_data.I.regs[rs1];

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_XOR",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_OR(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b100 // func3 C.OR
            or (data >> 10) & 0b11 != 0b11 //
            or (data >> 12 & 1 != 0) //
            or (data >> 5 & 0b11) != 0b10)
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);
            const rd: u5 = base.PopularIntReg.from_u3(@truncate(data >> 7)).to_reg().to_u5();
            const rs1: u5 = base.PopularIntReg.from_u3(@truncate(data >> 2)).to_reg().to_u5();

            hart_data.I.regs[rd] |= hart_data.I.regs[rs1];

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_OR",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_AND(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b100 // func3 C.AND
            or (data >> 10) & 0b11 != 0b11 //
            or (data >> 12 & 1 != 0) //
            or (data >> 5 & 0b11) != 0b11)
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);
            const rd: u5 = base.PopularIntReg.from_u3(@truncate(data >> 7)).to_reg().to_u5();
            const rs1: u5 = base.PopularIntReg.from_u3(@truncate(data >> 2)).to_reg().to_u5();

            hart_data.I.regs[rd] &= hart_data.I.regs[rs1];

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_AND",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_MV(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b10 // OP C2
            or (data >> 2) & 0b11111 == 0 //
            or (data >> 7) & 0b11111 == 0 //
            or ((data >> 12) & 1 != 0) //
            or data >> 13 != 0b100 // func3 C.MV
            )
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);
            const rs2: u5 = @truncate(data >> 2);
            const rd: u5 = @truncate(data >> 7);

            hart_data.I.regs[rd] = hart_data.I.regs[rs2];

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_MV",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_ADD(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b10 // OP C2
            or (data >> 2) & 0b11111 == 0 //
            or (data >> 7) & 0b11111 == 0 //
            or ((data >> 12) & 1 != 1) //
            or data >> 13 != 0b100 // func3 C.ADD
            )
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);
            const rs2: u5 = @truncate(data >> 2);
            const rd: u5 = @truncate(data >> 7);

            hart_data.I.regs[rd] +%= hart_data.I.regs[rs2];

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_ADD",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_SUBW(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b100 // func3 C.SUBW
            or (data >> 10) & 0b11 != 0b11 //
            or (data >> 12 & 1 != 1) //
            or (data >> 5 & 0b11) != 0b00)
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            if (ARCH == .X32) unreachable;
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);
            const rd: u5 = base.PopularIntReg.from_u3(@truncate(data >> 7)).to_reg().to_u5();
            const rs1: u5 = base.PopularIntReg.from_u3(@truncate(data >> 2)).to_reg().to_u5();

            hart_data.I.regs[rd] = @bitCast(@as(ARCH.iarch(), @as(i32, @bitCast(@as(u32, @truncate(hart_data.I.regs[rd])) -% @as(u32, @bitCast(@as(i32, @bitCast(@as(u32, @truncate(hart_data.I.regs[rs1]))))))))));

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_SUBW",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_ADDW(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b100 // func3 C.SUBW
            or (data >> 10) & 0b11 != 0b11 //
            or (data >> 12 & 1 != 1) //
            or (data >> 5 & 0b11) != 0b01)
                return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            if (ARCH == .X32) unreachable;
            _ = eei_data;
            debug.assert(instr_data.len == 2);
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);
            const rd: u5 = base.PopularIntReg.from_u3(@truncate(data >> 7)).to_reg().to_u5();
            const rs1: u5 = base.PopularIntReg.from_u3(@truncate(data >> 2)).to_reg().to_u5();

            hart_data.I.regs[rd] = @bitCast(@as(ARCH.iarch(), @as(i32, @bitCast(@as(u32, @truncate(hart_data.I.regs[rd])) +% @as(u32, @bitCast(@as(i32, @bitCast(@as(u32, @truncate(hart_data.I.regs[rs1]))))))))));

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_ADDW",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn C_NOP(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 2) return false;
            const data = std.mem.readInt(u16, @ptrCast(instr_data), .little);

            if (data & 0b11 != 0b01 // OP C1
            or data >> 13 != 0b000 // func3 C.NOP
            or (data >> 7) & std.math.maxInt(u5) != 0) return false;

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;
            debug.assert(instr_data.len == 2);

            hart_data.I.pc +%= 2;
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .name = "C_NOP",
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn buildInstrs(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) [33]Instruction(ARCH, DataEEI, DataHart) {
    return .{
        C_LWSP(ARCH, DataEEI, DataHart).instr(),
        C_LDSP(ARCH, DataEEI, DataHart).instr(),
        C_SWSP(ARCH, DataEEI, DataHart).instr(),
        C_SDSP(ARCH, DataEEI, DataHart).instr(),
        C_LW(ARCH, DataEEI, DataHart).instr(),
        C_LD(ARCH, DataEEI, DataHart).instr(),
        C_SW(ARCH, DataEEI, DataHart).instr(),
        C_SD(ARCH, DataEEI, DataHart).instr(),
        C_J(ARCH, DataEEI, DataHart).instr(),
        C_JAL(ARCH, DataEEI, DataHart).instr(),
        C_JR(ARCH, DataEEI, DataHart).instr(),
        C_JALR(ARCH, DataEEI, DataHart).instr(),
        C_BEQZ(ARCH, DataEEI, DataHart).instr(),
        C_BNEZ(ARCH, DataEEI, DataHart).instr(),
        C_LI(ARCH, DataEEI, DataHart).instr(),
        C_LUI(ARCH, DataEEI, DataHart).instr(),
        C_ADDI(ARCH, DataEEI, DataHart).instr(),
        C_ADDIW(ARCH, DataEEI, DataHart).instr(),
        C_ADDI16SPN(ARCH, DataEEI, DataHart).instr(),
        C_ADDI4SPN(ARCH, DataEEI, DataHart).instr(),
        C_SLLI(ARCH, DataEEI, DataHart).instr(),
        C_SRLI(ARCH, DataEEI, DataHart).instr(),
        C_SRAI(ARCH, DataEEI, DataHart).instr(),
        C_ANDI(ARCH, DataEEI, DataHart).instr(),
        C_SUB(ARCH, DataEEI, DataHart).instr(),
        C_XOR(ARCH, DataEEI, DataHart).instr(),
        C_OR(ARCH, DataEEI, DataHart).instr(),
        C_AND(ARCH, DataEEI, DataHart).instr(),
        C_SUBW(ARCH, DataEEI, DataHart).instr(),
        C_ADDW(ARCH, DataEEI, DataHart).instr(),
        C_NOP(ARCH, DataEEI, DataHart).instr(),
        C_MV(ARCH, DataEEI, DataHart).instr(),
        C_ADD(ARCH, DataEEI, DataHart).instr(),
    };
}
