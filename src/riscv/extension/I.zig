const std = @import("std");
const base = @import("../base.zig");

const rearrange = base.rearrange;

const debug = std.debug;

const Arch = base.Arch;
const IFX32 = base.InstrFormatX32;

const Instruction = base.Instruction;

pub fn buildDataHart(comptime ARCH: base.Arch) type {
    const uarch = ARCH.uarch();

    return struct {
        pc: uarch,
        regs: [32]uarch,
    };
}

pub fn LUI(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const data = std.mem.readInt(u32, @ptrCast(instr_data), .little);

            if (data & 0b1111111 != 0b0110111) return false; // LUI opcode

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const u = x32_instr.u;

            if (u.rd != 0) {
                hart_data.I.regs[u.rd] = @bitCast(@as(iarch, @as(i32, @bitCast(@as(u32, u.imm_31_12) << 12))));
            }

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

pub fn AUIPC(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const data = std.mem.readInt(u32, @ptrCast(instr_data), .little);

            if (data & 0b1111111 != 0b0010111) return false; // AUIPC opcode

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const u = x32_instr.u;

            const offset: uarch = @bitCast(@as(iarch, @as(i32, @bitCast(@as(u32, u.imm_31_12) << 12))));
            if (u.rd != 0) {
                hart_data.I.regs[u.rd] = hart_data.I.pc +% offset;
            }

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

pub fn JAL(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const data = std.mem.readInt(u32, @ptrCast(instr_data), .little);

            if (data & 0b1111111 != 0b1101111) return false; // JAL opcode

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const jimm = x32_instr.jimm;

            const pc = hart_data.I.pc + 4;
            hart_data.I.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(hart_data.I.pc)) + (@as(iarch, @as(i21, @bitCast(rearrange(u20, u21, jimm.imm, &base.@"imm_20|10:1|11|19:12")))))));

            if (pc - 4 == hart_data.I.pc) {
                hart_data.illegal_instruction();
                return;
            }

            if (jimm.rd != 0) {
                hart_data.I.regs[jimm.rd] = pc;
            }
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn JALR(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1100111) return false; // JALR opcode
            if (x32_instr.i.funct3 != 0b000) return false; // JALR func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;

            const pc = hart_data.I.pc + 4;
            hart_data.I.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(hart_data.I.regs[i.rs1])) + (@as(iarch, @as(i12, @bitCast(i.imm_11_0))))));
            hart_data.I.pc ^= hart_data.I.pc & 1;
            if (i.rd != 0) {
                hart_data.I.regs[i.rd] = pc;
            }
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn BEQ(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1100011) return false; // BRANCH opcode
            if (x32_instr.b.funct3 != 0b000) return false; // BEQ func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const b = x32_instr.b;
            if (hart_data.I.regs[b.rs1] == hart_data.I.regs[b.rs2]) {
                hart_data.I.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(hart_data.I.pc)) + b.get_imm()));
                return;
            }

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

pub fn BNE(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1100011) return false; // BRANCH opcode
            if (x32_instr.b.funct3 != 0b001) return false; // BNE func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const b = x32_instr.b;
            if (hart_data.I.regs[b.rs1] != hart_data.I.regs[b.rs2]) {
                hart_data.I.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(hart_data.I.pc)) + b.get_imm()));
                return;
            }

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

pub fn BLT(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1100011) return false; // BRANCH opcode
            if (x32_instr.b.funct3 != 0b100) return false; // BLT func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const b = x32_instr.b;
            if (@as(iarch, @bitCast(hart_data.I.regs[b.rs1])) < @as(iarch, @bitCast(hart_data.I.regs[b.rs2]))) {
                hart_data.I.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(hart_data.I.pc)) + b.get_imm()));
                return;
            }

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

pub fn BGE(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1100011) return false; // BRANCH opcode
            if (x32_instr.b.funct3 != 0b101) return false; // BGE func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const b = x32_instr.b;
            if (@as(iarch, @bitCast(hart_data.I.regs[b.rs1])) >= @as(iarch, @bitCast(hart_data.I.regs[b.rs2]))) {
                hart_data.I.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(hart_data.I.pc)) + b.get_imm()));
                return;
            }

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
pub fn BLTU(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1100011) return false; // BRANCH opcode
            if (x32_instr.b.funct3 != 0b110) return false; // BLTU func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const b = x32_instr.b;
            if (hart_data.I.regs[b.rs1] < hart_data.I.regs[b.rs2]) {
                hart_data.I.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(hart_data.I.pc)) + b.get_imm()));
                return;
            }

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

pub fn BGEU(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1100011) return false; // BRANCH opcode
            if (x32_instr.b.funct3 != 0b111) return false; // BGEU func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const b = x32_instr.b;
            if (hart_data.I.regs[b.rs1] >= hart_data.I.regs[b.rs2]) {
                hart_data.I.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(hart_data.I.pc)) + b.get_imm()));
                return;
            }

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

pub fn LB(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0000011) return false; // LOAD opcode
            if (x32_instr.i.funct3 != 0b000) return false; // LB func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            debug.assert(instr_data.len == 4);

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;
            var buffer: [1]u8 = undefined;
            const offset: uarch = @bitCast(@as(iarch, @bitCast(hart_data.I.regs[i.rs1])) + @as(iarch, i.imm_11_0));

            if (!hart_data.read(eei_data, offset, &buffer)) return;

            hart_data.I.regs[i.rd] = @bitCast(@as(iarch, std.mem.readInt(i8, &buffer, .little)));

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

pub fn LH(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0000011) return false; // LOAD opcode
            if (x32_instr.i.funct3 != 0b001) return false; // LH func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;
            var buffer: [2]u8 = undefined;
            const offset: uarch = @bitCast(@as(iarch, @bitCast(hart_data.I.regs[i.rs1])) + @as(iarch, i.imm_11_0));

            if (!hart_data.read(eei_data, offset, &buffer)) return;

            hart_data.I.regs[i.rd] = @bitCast(@as(iarch, std.mem.readInt(i16, &buffer, .little)));

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

pub fn LW(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0000011) return false; // LOAD opcode
            if (x32_instr.i.funct3 != 0b010) return false; // LW func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;
            var buffer: [4]u8 = undefined;
            const offset: uarch = @bitCast(@as(iarch, @bitCast(hart_data.I.regs[i.rs1])) + @as(iarch, i.imm_11_0));

            if (!hart_data.read(eei_data, offset, &buffer)) return;

            hart_data.I.regs[i.rd] = @bitCast(@as(iarch, std.mem.readInt(i32, &buffer, .little)));

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

pub fn LBU(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0000011) return false; // LOAD opcode
            if (x32_instr.i.funct3 != 0b100) return false; // LBU func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;
            var buffer: [1]u8 = undefined;
            const offset: uarch = @bitCast(@as(iarch, @bitCast(hart_data.I.regs[i.rs1])) + @as(iarch, i.imm_11_0));

            if (!hart_data.read(eei_data, offset, &buffer)) return;

            hart_data.I.regs[i.rd] = std.mem.readInt(u8, &buffer, .little);

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

pub fn LHU(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0000011) return false; // LOAD opcode
            if (x32_instr.i.funct3 != 0b101) return false; // LHU func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;
            var buffer: [2]u8 = undefined;
            const offset: uarch = @bitCast(@as(iarch, @bitCast(hart_data.I.regs[i.rs1])) + @as(iarch, i.imm_11_0));

            if (!hart_data.read(eei_data, offset, &buffer)) return;

            hart_data.I.regs[i.rd] = std.mem.readInt(u16, &buffer, .little);

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

pub fn SB(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0100011) return false; // STORE opcode
            if (x32_instr.s.funct3 != 0b000) return false; // SB func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const s = x32_instr.s;
            var buffer: [1]u8 = undefined;
            std.mem.writeInt(u8, &buffer, @truncate(hart_data.I.regs[s.rs2]), .little);
            const offset: uarch = @bitCast(@as(iarch, @bitCast(hart_data.I.regs[s.rs1])) + @as(iarch, s.get_imm()));

            if (!hart_data.write(eei_data, offset, &buffer)) return;

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

pub fn SH(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0100011) return false; // STORE opcode
            if (x32_instr.s.funct3 != 0b001) return false; // SH func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const s = x32_instr.s;
            var buffer: [2]u8 = undefined;
            std.mem.writeInt(u16, &buffer, @truncate(hart_data.I.regs[s.rs2]), .little);
            const offset: uarch = @bitCast(@as(iarch, @bitCast(hart_data.I.regs[s.rs1])) + @as(iarch, s.get_imm()));

            if (!hart_data.write(eei_data, offset, &buffer)) return;

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

pub fn SW(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0100011) return false; // STORE opcode
            if (x32_instr.s.funct3 != 0b010) return false; // SW func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const s = x32_instr.s;
            var buffer: [4]u8 = undefined;
            std.mem.writeInt(u32, &buffer, @truncate(hart_data.I.regs[s.rs2]), .little);
            const offset: uarch = @bitCast(@as(iarch, @bitCast(hart_data.I.regs[s.rs1])) + @as(iarch, s.get_imm()));

            if (!hart_data.write(eei_data, offset, &buffer)) return;

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

pub fn ADDI(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0010011) return false; // OP-IMM opcode
            if (x32_instr.i.funct3 != 0b000) return false; // SW func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;

            if (i.rd != 0) {
                hart_data.I.regs[i.rd] = hart_data.I.regs[i.rs1] +% @as(uarch, @bitCast(@as(iarch, i.imm_11_0)));
            }

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

pub fn SLTI(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0010011) return false; // OP-IMM opcode
            if (x32_instr.i.funct3 != 0b010) return false; // SLTI func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;

            if (i.rd != 0) {
                hart_data.I.regs[i.rd] = if (@as(iarch, @bitCast(hart_data.I.regs[i.rs1])) < @as(iarch, i.imm_11_0)) 1 else 0;
            }

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

pub fn SLTIU(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0010011) return false; // OP-IMM opcode
            if (x32_instr.i.funct3 != 0b011) return false; // SLTIU func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;

            if (i.rd != 0) {
                hart_data.I.regs[i.rd] = if (hart_data.I.regs[i.rs1] < @as(uarch, @bitCast(@as(iarch, i.imm_11_0)))) 1 else 0;
            }

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

pub fn XORI(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0010011) return false; // OP-IMM opcode
            if (x32_instr.i.funct3 != 0b100) return false; // XORI func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;

            if (i.rd != 0) {
                hart_data.I.regs[i.rd] = hart_data.I.regs[i.rs1] ^ @as(uarch, @bitCast(@as(iarch, i.imm_11_0)));
            }

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

pub fn ORI(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0010011) return false; // OP-IMM opcode
            if (x32_instr.i.funct3 != 0b110) return false; // ORI func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;

            if (i.rd != 0) {
                hart_data.I.regs[i.rd] = hart_data.I.regs[i.rs1] | @as(uarch, @bitCast(@as(iarch, i.imm_11_0)));
            }

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

pub fn ANDI(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0010011) return false; // OP-IMM opcode
            if (x32_instr.i.funct3 != 0b111) return false; // ANDI func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;

            if (i.rd != 0) {
                hart_data.I.regs[i.rd] = hart_data.I.regs[i.rs1] & @as(uarch, @bitCast(@as(iarch, i.imm_11_0)));
            }

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

pub fn SLLI(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0010011) return false; // OP-IMM opcode
            if (x32_instr.i_1.funct3 != 0b001) return false; // SLLI func3
            if (x32_instr.i_1.op != 0b000000) return false; // SLLI op
            if (ARCH == .X32) {
                if (x32_instr.i_1.shamt & 0b100000 != 0b0) return false; // SLLI shamt needs to be u5 in X32
            }

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i_1 = x32_instr.i_1;

            if (i_1.rd != 0) {
                hart_data.I.regs[i_1.rd] = hart_data.I.regs[i_1.rs1] << @truncate(i_1.shamt);
            }

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

pub fn SRLI(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0010011) return false; // OP-IMM opcode
            if (x32_instr.i_1.funct3 != 0b101) return false; // SRLI func3
            if (x32_instr.i_1.op != 0b000000) return false; // SRLI op
            if (ARCH == .X32) {
                if (x32_instr.i_1.shamt & 0b100000 != 0b0) return false; // SRLI shamt needs to be u5 in X32
            }

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i_1 = x32_instr.i_1;

            if (i_1.rd != 0) {
                hart_data.I.regs[i_1.rd] = hart_data.I.regs[i_1.rs1] >> @truncate(i_1.shamt);
            }

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

pub fn SRAI(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0010011) return false; // OP-IMM opcode
            if (x32_instr.i_1.funct3 != 0b101) return false; // SRLI func3
            if (x32_instr.i_1.op != 0b010000) return false; // SRAI op
            if (ARCH == .X32) {
                if (x32_instr.i_1.shamt & 0b100000 != 0b0) return false; // SRAI shamt needs to be u5 in X32
            }

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i_1 = x32_instr.i_1;

            if (i_1.rd != 0) {
                const sign_bit: uarch = comptime 1 << (ARCH.bytes() - 1);
                const sign: bool = hart_data.I.regs[i_1.rs1] & sign_bit == sign_bit;
                hart_data.I.regs[i_1.rd] = hart_data.I.regs[i_1.rs1] >> @truncate(i_1.shamt);

                if (sign) {
                    const MAX: uarch = std.math.maxInt(uarch);
                    hart_data.I.regs[i_1.rd] |= MAX ^ (MAX >> @truncate(i_1.shamt));
                }
            }

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

pub fn ADD(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0110011) return false; // OP opcode
            if (x32_instr.r.funct3 != 0b000) return false; // ADD func3
            if (x32_instr.r.funct7 != 0b0000000) return false; // ADD func7

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const r = x32_instr.r;

            if (r.rd != 0) {
                hart_data.I.regs[r.rd] = hart_data.I.regs[r.rs1] +% hart_data.I.regs[r.rs2];
            }

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

pub fn SUB(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0110011) return false; // OP opcode
            if (x32_instr.r.funct3 != 0b000) return false; // ADD func3
            if (x32_instr.r.funct7 != 0b0100000) return false; // SUB func7

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const r = x32_instr.r;

            if (r.rd != 0) {
                hart_data.I.regs[r.rd] = hart_data.I.regs[r.rs1] -% hart_data.I.regs[r.rs2];
            }

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

pub fn SLL(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0110011) return false; // OP opcode
            if (x32_instr.r.funct3 != 0b001) return false; // SLL func3
            if (x32_instr.r.funct7 != 0b0000000) return false; // SLL func7

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const r = x32_instr.r;

            if (r.rd != 0) {
                hart_data.I.regs[r.rd] = hart_data.I.regs[r.rs1] << @truncate(hart_data.I.regs[r.rs2]);
            }

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

pub fn SLT(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0110011) return false; // OP opcode
            if (x32_instr.r.funct3 != 0b010) return false; // SLT func3
            if (x32_instr.r.funct7 != 0b0000000) return false; // SLT func7

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const r = x32_instr.r;

            if (r.rd != 0) {
                hart_data.I.regs[r.rd] = if (@as(iarch, @bitCast(hart_data.I.regs[r.rs1])) < @as(iarch, @bitCast(hart_data.I.regs[r.rs2]))) 1 else 0;
            }

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

pub fn SLTU(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0110011) return false; // OP opcode
            if (x32_instr.r.funct3 != 0b011) return false; // SLTU func3
            if (x32_instr.r.funct7 != 0b0000000) return false; // SLTU func7

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const r = x32_instr.r;

            if (r.rd != 0) {
                hart_data.I.regs[r.rd] = if (hart_data.I.regs[r.rs1] < hart_data.I.regs[r.rs2]) 1 else 0;
            }

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

pub fn XOR(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0110011) return false; // OP opcode
            if (x32_instr.r.funct3 != 0b100) return false; // XOR func3
            if (x32_instr.r.funct7 != 0b0000000) return false; // XOR func7

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const r = x32_instr.r;

            if (r.rd != 0) {
                hart_data.I.regs[r.rd] = hart_data.I.regs[r.rs1] ^ hart_data.I.regs[r.rs2];
            }

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

pub fn SRL(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0110011) return false; // OP opcode
            if (x32_instr.r.funct3 != 0b101) return false; // SRL func3
            if (x32_instr.r.funct7 != 0b0000000) return false; // SRL func7

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const r = x32_instr.r;

            if (r.rd != 0) {
                hart_data.I.regs[r.rd] = hart_data.I.regs[r.rs1] >> @truncate(hart_data.I.regs[r.rs2]);
            }

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

pub fn SRA(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0110011) return false; // OP opcode
            if (x32_instr.r.funct3 != 0b101) return false; // SRL func3
            if (x32_instr.r.funct7 != 0b0100000) return false; // SRA func7

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const r = x32_instr.r;

            if (r.rd != 0) {
                const sign_bit: uarch = comptime 1 << (ARCH.bytes() - 1);
                const sign: bool = hart_data.I.regs[r.rs1] & sign_bit == sign_bit;

                const shift: uarch = hart_data.I.regs[r.rs2];
                hart_data.I.regs[r.rd] = hart_data.I.regs[r.rs1] >> @truncate(shift);

                if (sign) {
                    const MAX: uarch = std.math.maxInt(uarch);
                    hart_data.I.regs[r.rd] |= MAX ^ (MAX >> @truncate(shift));
                }
            }

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

pub fn OR(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0110011) return false; // OP opcode
            if (x32_instr.r.funct3 != 0b110) return false; // OR func3
            if (x32_instr.r.funct7 != 0b0000000) return false; // OR func7

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const r = x32_instr.r;

            if (r.rd != 0) {
                hart_data.I.regs[r.rd] = hart_data.I.regs[r.rs1] | hart_data.I.regs[r.rs2];
            }

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

pub fn AND(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0110011) return false; // OP opcode
            if (x32_instr.r.funct3 != 0b111) return false; // AND func3
            if (x32_instr.r.funct7 != 0b0000000) return false; // AND func7

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const r = x32_instr.r;

            if (r.rd != 0) {
                hart_data.I.regs[r.rd] = hart_data.I.regs[r.rs1] & hart_data.I.regs[r.rs2];
            }

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

pub fn FENCE(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0001111) return false; // MISC-MEM opcode
            if (x32_instr.f.func3 != 0b000) return false; // FENCE func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            debug.assert(instr_data.len == 4);

            hart_data.fence(eei_data);
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn ECALL(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1110011) return false; // SYSTEM opcode
            if (x32_instr.s.imm_4_0 != 0b00000) return false; // ECALL imm_4_0
            if (x32_instr.s.funct3 != 0b000) return false; // ECALL func3
            if (x32_instr.s.rs1 != 0b00000) return false; // ECALL rs1
            if (x32_instr.s.rs2 != 0b00000) return false; // ECALL rs2
            if (x32_instr.s.imm_11_5 != 0b0000000) return false; // ECALL imm_11_5

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            debug.assert(instr_data.len == 4);

            hart_data.ecall(eei_data);
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn EBREAK(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1110011) return false; // SYSTEM opcode
            if (x32_instr.s.imm_4_0 != 0b00000) return false; // EBREAK imm_4_0
            if (x32_instr.s.funct3 != 0b000) return false; // EBREAK func3
            if (x32_instr.s.rs1 != 0b00000) return false; // EBREAK rs1
            if (x32_instr.s.rs2 != 0b00001) return false; // EBREAK rs2
            if (x32_instr.s.imm_11_5 != 0b0000000) return false; // EBREAK imm_11_5

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            debug.assert(instr_data.len == 4);

            hart_data.ebreak(eei_data);
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn LWU(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0000011) return false; // LOAD opcode
            if (x32_instr.i.funct3 != 0b110) return false; // LWU func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;
            var buffer: [4]u8 = undefined;
            const offset: uarch = @bitCast(@as(iarch, @bitCast(hart_data.I.regs[i.rs1])) + @as(iarch, i.imm_11_0));

            if (!hart_data.read(eei_data, offset, &buffer)) return;

            hart_data.I.regs[i.rd] = std.mem.readInt(u32, &buffer, .little);

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

pub fn LD(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0000011) return false; // LOAD opcode
            if (x32_instr.i.funct3 != 0b011) return false; // LD func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            if (ARCH == .X32) @panic("X64 instruction");
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;
            var buffer: [8]u8 = undefined;
            const offset: uarch = @bitCast(@as(iarch, @bitCast(hart_data.I.regs[i.rs1])) + @as(iarch, i.imm_11_0));

            if (!hart_data.read(eei_data, offset, &buffer)) return;

            hart_data.I.regs[i.rd] = @bitCast(@as(iarch, std.mem.readInt(i64, &buffer, .little)));

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

pub fn SD(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0100011) return false; // STORE opcode
            if (x32_instr.s.funct3 != 0b011) return false; // SD func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            if (ARCH == .X32) @panic("X64 instruction");
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const s = x32_instr.s;
            var buffer: [8]u8 = undefined;
            std.mem.writeInt(u64, &buffer, @truncate(hart_data.I.regs[s.rs2]), .little);
            const offset: uarch = @bitCast(@as(iarch, @bitCast(hart_data.I.regs[s.rs1])) + @as(iarch, s.get_imm()));

            if (!hart_data.write(eei_data, offset, &buffer)) return;

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

pub fn ADDIW(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0011011) return false; // OP-IMM-32 opcode
            if (x32_instr.i.funct3 != 0b000) return false; // SW func3

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            if (ARCH == .X32) @panic("X64 instruction");
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;

            if (i.rd != 0) {
                hart_data.I.regs[i.rd] = @bitCast(@as(iarch, @as(i32, @truncate(@as(isize, @bitCast(hart_data.I.regs[i.rs1])))) +% @as(i32, i.imm_11_0)));
            }

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

pub fn SLLIW(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0011011) return false; // OP-IMM-32 opcode
            if (x32_instr.i_1.funct3 != 0b001) return false; // SLLI func3
            if (x32_instr.i_1.op != 0b000000) return false; // SLLI op
            if (ARCH == .X32) {
                if (x32_instr.i_1.shamt & 0b100000 != 0b0) return false; // SLLI shamt needs to be u5 in X32
            }

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            if (ARCH == .X32) @panic("X64 instruction");
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i_1 = x32_instr.i_1;

            if (i_1.rd != 0) {
                hart_data.I.regs[i_1.rd] = @bitCast(@as(iarch, @as(i32, @truncate(@as(iarch, @bitCast(hart_data.I.regs[i_1.rs1])))) << @truncate(i_1.shamt)));
            }

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

pub fn SRLIW(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0011011) return false; // OP-IMM-32 opcode
            if (x32_instr.i_1.funct3 != 0b101) return false; // SRLI func3
            if (x32_instr.i_1.op != 0b000000) return false; // SRLI op
            if (ARCH == .X32) {
                if (x32_instr.i_1.shamt & 0b100000 != 0b0) return false; // SRLI shamt needs to be u5 in X32
            }

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            if (ARCH == .X32) @panic("X64 instruction");
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i_1 = x32_instr.i_1;

            if (i_1.rd != 0) {
                hart_data.I.regs[i_1.rd] = @bitCast(@as(iarch, @as(i32, @bitCast(@as(u32, @bitCast(@as(i32, @truncate(@as(iarch, @bitCast(hart_data.I.regs[i_1.rs1])))))) >> @truncate(i_1.shamt)))));
            }

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

pub fn SRAIW(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0011011) return false; // OP-IMM-32 opcode
            if (x32_instr.i_1.funct3 != 0b101) return false; // SRLI func3
            if (x32_instr.i_1.op != 0b010000) return false; // SRAI op
            if (ARCH == .X32) {
                if (x32_instr.i_1.shamt & 0b100000 != 0b0) return false; // SRAI shamt needs to be u5 in X32
            }

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            if (ARCH == .X32) @panic("X64 instruction");
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i_1 = x32_instr.i_1;

            if (i_1.rd != 0) {
                const sign_bit: uarch = comptime 1 << 31;
                const sign: bool = hart_data.I.regs[i_1.rs1] & sign_bit == sign_bit;
                hart_data.I.regs[i_1.rd] = @bitCast(@as(iarch, @as(i32, @bitCast(@as(u32, @bitCast(@as(i32, @truncate(@as(iarch, @bitCast(hart_data.I.regs[i_1.rs1])))))) >> @truncate(i_1.shamt)))));

                if (sign) {
                    const MAX: uarch = std.math.maxInt(u32);
                    hart_data.I.regs[i_1.rd] |= MAX - (MAX >> @truncate(i_1.shamt));
                }

                hart_data.I.regs[i_1.rd] = @bitCast(@as(iarch, @as(i32, @truncate(@as(iarch, @bitCast(hart_data.I.regs[i_1.rd]))))));
            }

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

pub fn ADDW(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0111011) return false; // OP-32 opcode
            if (x32_instr.r.funct3 != 0b000) return false; // ADD func3
            if (x32_instr.r.funct7 != 0b0000000) return false; // ADD func7

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            if (ARCH == .X32) @panic("X64 instruction");
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const r = x32_instr.r;

            if (r.rd != 0) {
                hart_data.I.regs[r.rd] = @bitCast(@as(iarch, @as(i32, @truncate(@as(iarch, @bitCast(hart_data.I.regs[r.rs1])))) +% @as(i32, @truncate(@as(iarch, @bitCast(hart_data.I.regs[r.rs2]))))));
            }

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

pub fn SUBW(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;
            if (instr_data.len != 4) return false;

            if (ARCH == .X32) @panic("X64 instruction");
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0111011) return false; // OP-32 opcode
            if (x32_instr.r.funct3 != 0b000) return false; // ADD func3
            if (x32_instr.r.funct7 != 0b0100000) return false; // SUB func7

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const r = x32_instr.r;

            if (r.rd != 0) {
                hart_data.I.regs[r.rd] = @bitCast(@as(iarch, @as(i32, @truncate(@as(iarch, @bitCast(hart_data.I.regs[r.rs1])))) -% @as(i32, @truncate(@as(iarch, @bitCast(hart_data.I.regs[r.rs2]))))));
            }

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

pub fn SLLW(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0111011) return false; // OP-32 opcode
            if (x32_instr.r.funct3 != 0b001) return false; // SLL func3
            if (x32_instr.r.funct7 != 0b0000000) return false; // SLL func7

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            if (ARCH == .X32) @panic("X64 instruction");
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const r = x32_instr.r;
            const value: u32 = @bitCast(@as(i32, @truncate(@as(iarch, @bitCast(hart_data.I.regs[r.rs1])))));

            if (r.rd != 0) {
                hart_data.I.regs[r.rd] = @bitCast(@as(iarch, @as(i32, @bitCast(value << @truncate(hart_data.I.regs[r.rs2])))));
            }

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

pub fn SRLW(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0111011) return false; // OP-32 opcode
            if (x32_instr.r.funct3 != 0b101) return false; // SRL func3
            if (x32_instr.r.funct7 != 0b0000000) return false; // SRL func7

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            if (ARCH == .X32) @panic("X64 instruction");
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const r = x32_instr.r;
            const value: u32 = @bitCast(@as(i32, @truncate(@as(iarch, @bitCast(hart_data.I.regs[r.rs1])))));

            if (r.rd != 0) {
                hart_data.I.regs[r.rd] = @bitCast(@as(iarch, @as(i32, @bitCast(value >> @truncate(hart_data.I.regs[r.rs2])))));
            }

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

pub fn SRAW(comptime ARCH: base.Arch, comptime DataEEI: type, comptime DataHart: type) type {
    const uarch = ARCH.uarch();
    const iarch = ARCH.iarch();

    return struct {
        fn check(instr_data: []const u8) bool {
            if (ARCH == .X32) return false;
            if (instr_data.len != 4) return false;

            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b0111011) return false; // OP-32 opcode
            if (x32_instr.r.funct3 != 0b101) return false; // SRL func3
            if (x32_instr.r.funct7 != 0b0100000) return false; // SRA func7

            return true;
        }

        fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            if (ARCH == .X32) @panic("X64 instruction");
            debug.assert(instr_data.len == 4);
            const x32_instr: IFX32 = @bitCast(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const r = x32_instr.r;

            if (r.rd != 0) {
                const sign_bit: uarch = comptime 1 << 31;
                const value: u32 = @bitCast(@as(i32, @truncate(@as(iarch, @bitCast(hart_data.I.regs[r.rs1])))));
                const sign: bool = value & sign_bit == sign_bit;
                const shift: u5 = @truncate(hart_data.I.regs[r.rs2]);

                hart_data.I.regs[r.rd] = value >> shift;

                if (sign) {
                    const MAX: uarch = std.math.maxInt(u32);
                    hart_data.I.regs[r.rd] |= MAX - (MAX >> shift);
                }

                hart_data.I.regs[r.rd] = @bitCast(@as(iarch, @as(i32, @bitCast(@as(u32, @truncate(hart_data.I.regs[r.rd]))))));
            }

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

pub fn buildInstrs(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) [52]Instruction(ARCH, DataEEI, DataHart) {
    return .{
        LUI(ARCH, DataEEI, DataHart).instr(),
        AUIPC(ARCH, DataEEI, DataHart).instr(),
        JAL(ARCH, DataEEI, DataHart).instr(),
        JALR(ARCH, DataEEI, DataHart).instr(),
        BEQ(ARCH, DataEEI, DataHart).instr(),
        BNE(ARCH, DataEEI, DataHart).instr(),
        BLT(ARCH, DataEEI, DataHart).instr(),
        BGE(ARCH, DataEEI, DataHart).instr(),
        BLTU(ARCH, DataEEI, DataHart).instr(),
        BGEU(ARCH, DataEEI, DataHart).instr(),
        LB(ARCH, DataEEI, DataHart).instr(),
        LH(ARCH, DataEEI, DataHart).instr(),
        LW(ARCH, DataEEI, DataHart).instr(),
        LBU(ARCH, DataEEI, DataHart).instr(),
        LHU(ARCH, DataEEI, DataHart).instr(),
        SB(ARCH, DataEEI, DataHart).instr(),
        SH(ARCH, DataEEI, DataHart).instr(),
        SW(ARCH, DataEEI, DataHart).instr(),
        ADDI(ARCH, DataEEI, DataHart).instr(),
        SLTI(ARCH, DataEEI, DataHart).instr(),
        SLTIU(ARCH, DataEEI, DataHart).instr(),
        XORI(ARCH, DataEEI, DataHart).instr(),
        ORI(ARCH, DataEEI, DataHart).instr(),
        ANDI(ARCH, DataEEI, DataHart).instr(),
        SLLI(ARCH, DataEEI, DataHart).instr(),
        SRLI(ARCH, DataEEI, DataHart).instr(),
        SRAI(ARCH, DataEEI, DataHart).instr(),
        ADD(ARCH, DataEEI, DataHart).instr(),
        SUB(ARCH, DataEEI, DataHart).instr(),
        SLL(ARCH, DataEEI, DataHart).instr(),
        SLT(ARCH, DataEEI, DataHart).instr(),
        SLTU(ARCH, DataEEI, DataHart).instr(),
        XOR(ARCH, DataEEI, DataHart).instr(),
        SRL(ARCH, DataEEI, DataHart).instr(),
        SRA(ARCH, DataEEI, DataHart).instr(),
        OR(ARCH, DataEEI, DataHart).instr(),
        AND(ARCH, DataEEI, DataHart).instr(),
        FENCE(ARCH, DataEEI, DataHart).instr(),
        ECALL(ARCH, DataEEI, DataHart).instr(),
        EBREAK(ARCH, DataEEI, DataHart).instr(),
        LWU(ARCH, DataEEI, DataHart).instr(),
        LD(ARCH, DataEEI, DataHart).instr(),
        SD(ARCH, DataEEI, DataHart).instr(),
        ADDIW(ARCH, DataEEI, DataHart).instr(),
        SLLIW(ARCH, DataEEI, DataHart).instr(),
        SRLIW(ARCH, DataEEI, DataHart).instr(),
        SRAIW(ARCH, DataEEI, DataHart).instr(),
        ADDW(ARCH, DataEEI, DataHart).instr(),
        SUBW(ARCH, DataEEI, DataHart).instr(),
        SLLW(ARCH, DataEEI, DataHart).instr(),
        SRLW(ARCH, DataEEI, DataHart).instr(),
        SRAW(ARCH, DataEEI, DataHart).instr(),
    };
}
