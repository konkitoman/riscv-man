const std = @import("std");
const log = std.log;
const base = @import("base.zig");

const IF16 = base.InstrFormatX16;
const IF32 = base.InstrFormatX32;
const IR = base.IntReg;
const IRf = IR.from_u5;
const PIR = base.PopularIntReg;
const PIRf = PIR.from_u3;

// # is for when are declared
// a is for assembly helpers
// w is for writing to memory
// f is for format

inline fn cast(T: type, value: anytype) T {
    return @bitCast(value);
}

inline fn t(T: type, value: anytype) T {
    return @truncate(value);
}

inline fn s(T: type, value: anytype) T {
    return @as(T, value);
}

pub const RV32I = union(enum) {
    LUI: struct { rd: IR, imm: u20 },
    AUIPC: struct { rd: IR, imm: u20 },
    JAL: struct { rd: IR, imm: i20 },
    JALR: struct { rd: IR, rs1: IR, imm: i12 },
    BEQ: struct { rs1: IR, rs2: IR, offset: i12 },
    BNE: struct { rs1: IR, rs2: IR, offset: i12 },
    BLT: struct { rs1: IR, rs2: IR, offset: i12 },
    BGE: struct { rs1: IR, rs2: IR, offset: i12 },
    BLTU: struct { rs1: IR, rs2: IR, offset: i12 },
    BGEU: struct { rs1: IR, rs2: IR, offset: i12 },
    LB: struct { rd: IR, rs1: IR, offset: i12 },
    LH: struct { rd: IR, rs1: IR, offset: i12 },
    LW: struct { rd: IR, rs1: IR, offset: i12 },
    LBU: struct { rd: IR, rs1: IR, offset: i12 },
    LHU: struct { rd: IR, rs1: IR, offset: i12 },
    SB: struct { rs1: IR, rs2: IR, offset: i12 },
    SH: struct { rs1: IR, rs2: IR, offset: i12 },
    SW: struct { rs1: IR, rs2: IR, offset: i12 },
    ADDI: struct { rd: IR, rs1: IR, imm: i12 },
    SLTI: struct { rd: IR, rs1: IR, imm: i12 },
    SLTIU: struct { rd: IR, rs1: IR, imm: i12 },
    XORI: struct { rd: IR, rs1: IR, imm: i12 },
    ORI: struct { rd: IR, rs1: IR, imm: i12 },
    ANDI: struct { rd: IR, rs1: IR, imm: i12 },
    SLLI: struct { rd: IR, rs1: IR, shamt: u5 },
    SRLI: struct { rd: IR, rs1: IR, shamt: u5 },
    SRAI: struct { rd: IR, rs1: IR, shamt: u5 },
    ADD: struct { rd: IR, rs1: IR, rs2: IR },
    SUB: struct { rd: IR, rs1: IR, rs2: IR },
    SLL: struct { rd: IR, rs1: IR, rs2: IR },
    SLT: struct { rd: IR, rs1: IR, rs2: IR },
    SLTU: struct { rd: IR, rs1: IR, rs2: IR },
    XOR: struct { rd: IR, rs1: IR, rs2: IR },
    SRL: struct { rd: IR, rs1: IR, rs2: IR },
    SRA: struct { rd: IR, rs1: IR, rs2: IR },
    OR: struct { rd: IR, rs1: IR, rs2: IR },
    AND: struct { rd: IR, rs1: IR, rs2: IR },
    FENCE: struct { rd: IR, rs1: IR, succ: base.FFlags, pred: base.FFlags, fm: u4 },
    FENCE_I,
    ECALL,
    EBREAK,
    URET,
    SRET,
    MRET,

    const Self = @This();

    pub fn to_memory(self: Self, memory: []u8) error{OutOfSpace}!usize {
        const vari = switch (self) {
            .LUI => |i| (base.InstrFormatX32{ .u = .{ .imm_31_12 = i.imm, .rd = i.rd.to_u5(), .opcode = 0b0110111 } }).to_varinstr(),
            .AUIPC => |i| (base.InstrFormatX32{ .u = .{ .imm_31_12 = i.imm, .rd = i.rd.to_u5(), .opcode = 0b0010111 } }).to_varinstr(),
            .JAL => |i| (base.InstrFormatX32{ .j = .{ .imm_10_1 = @truncate(cast(u20, i.imm)), .b_11 = @truncate(cast(u20, i.imm) >> 10), .imm_19_12 = @truncate((cast(u20, i.imm) >> 11)), .b_20 = @truncate(cast(u20, i.imm) >> 19), .rd = i.rd.to_u5(), .opcode = 0b1101111 } }).to_varinstr(),
            .JALR => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = i.imm, .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b1100111 } }).to_varinstr(),
            .BEQ => |i| (base.InstrFormatX32{ .b = .{ .imm_12 = @truncate((cast(u12, i.offset) >> 11) & 1), .imm_11 = @truncate(cast(u12, i.offset) >> 10), .imm_10_5 = @truncate(cast(u12, i.offset) >> 4), .imm_4_1 = @truncate(cast(u12, i.offset)), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .opcode = 0b1100011 } }).to_varinstr(),
            .BNE => |i| (base.InstrFormatX32{ .b = .{ .imm_12 = @truncate((cast(u12, i.offset) >> 11) & 1), .imm_11 = @truncate(cast(u12, i.offset) >> 10), .imm_10_5 = @truncate(cast(u12, i.offset) >> 4), .imm_4_1 = @truncate(cast(u12, i.offset)), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .opcode = 0b1100011 } }).to_varinstr(),
            .BLT => |i| (base.InstrFormatX32{ .b = .{ .imm_12 = @truncate((cast(u12, i.offset) >> 11) & 1), .imm_11 = @truncate(cast(u12, i.offset) >> 10), .imm_10_5 = @truncate(cast(u12, i.offset) >> 4), .imm_4_1 = @truncate(cast(u12, i.offset)), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b100, .opcode = 0b1100011 } }).to_varinstr(),
            .BGE => |i| (base.InstrFormatX32{ .b = .{ .imm_12 = @truncate((cast(u12, i.offset) >> 11) & 1), .imm_11 = @truncate(cast(u12, i.offset) >> 10), .imm_10_5 = @truncate(cast(u12, i.offset) >> 4), .imm_4_1 = @truncate(cast(u12, i.offset)), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .opcode = 0b1100011 } }).to_varinstr(),
            .BLTU => |i| (base.InstrFormatX32{ .b = .{ .imm_12 = @truncate((cast(u12, i.offset) >> 11) & 1), .imm_11 = @truncate(cast(u12, i.offset) >> 10), .imm_10_5 = @truncate(cast(u12, i.offset) >> 4), .imm_4_1 = @truncate(cast(u12, i.offset)), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b110, .opcode = 0b1100011 } }).to_varinstr(),
            .BGEU => |i| (base.InstrFormatX32{ .b = .{ .imm_12 = @truncate((cast(u12, i.offset) >> 11) & 1), .imm_11 = @truncate(cast(u12, i.offset) >> 10), .imm_10_5 = @truncate(cast(u12, i.offset) >> 4), .imm_4_1 = @truncate(cast(u12, i.offset)), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b111, .opcode = 0b1100011 } }).to_varinstr(),
            .LB => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = i.offset, .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b0000011 } }).to_varinstr(),
            .LH => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = i.offset, .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .rd = i.rd.to_u5(), .opcode = 0b0000011 } }).to_varinstr(),
            .LW => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = i.offset, .rs1 = i.rs1.to_u5(), .funct3 = 0b010, .rd = i.rd.to_u5(), .opcode = 0b0000011 } }).to_varinstr(),
            .LBU => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = i.offset, .rs1 = i.rs1.to_u5(), .funct3 = 0b100, .rd = i.rd.to_u5(), .opcode = 0b0000011 } }).to_varinstr(),
            .LHU => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = i.offset, .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0000011 } }).to_varinstr(),
            .SB => |i| (base.InstrFormatX32{ .s = .{ .imm_11_5 = @truncate(cast(u12, i.offset) >> 5), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .imm_4_0 = @truncate(cast(u12, i.offset)), .opcode = 0b0100011 } }).to_varinstr(),
            .SH => |i| (base.InstrFormatX32{ .s = .{ .imm_11_5 = @truncate(cast(u12, i.offset) >> 5), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .imm_4_0 = @truncate(cast(u12, i.offset)), .opcode = 0b0100011 } }).to_varinstr(),
            .SW => |i| (base.InstrFormatX32{ .s = .{ .imm_11_5 = @truncate(cast(u12, i.offset) >> 5), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b010, .imm_4_0 = @truncate(cast(u12, i.offset)), .opcode = 0b0100011 } }).to_varinstr(),
            .ADDI => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = i.imm, .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .SLTI => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = i.imm, .rs1 = i.rs1.to_u5(), .funct3 = 0b010, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .SLTIU => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = i.imm, .rs1 = i.rs1.to_u5(), .funct3 = 0b011, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .XORI => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = i.imm, .rs1 = i.rs1.to_u5(), .funct3 = 0b100, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .ORI => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = i.imm, .rs1 = i.rs1.to_u5(), .funct3 = 0b110, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .ANDI => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = i.imm, .rs1 = i.rs1.to_u5(), .funct3 = 0b111, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .SLLI => |i| (base.InstrFormatX32{ .i_1 = .{ .op = 0b000000, .shamt = i.shamt, .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .SRLI => |i| (base.InstrFormatX32{ .i_1 = .{ .op = 0b000000, .shamt = i.shamt, .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .SRAI => |i| (base.InstrFormatX32{ .i_1 = .{ .op = 0b010000, .shamt = i.shamt, .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .ADD => |i| (base.InstrFormatX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .SUB => |i| (base.InstrFormatX32{ .r = .{ .funct7 = 0b0100000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .SLL => |i| (base.InstrFormatX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .SLT => |i| (base.InstrFormatX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b010, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .SLTU => |i| (base.InstrFormatX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b011, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .XOR => |i| (base.InstrFormatX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b100, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .SRL => |i| (base.InstrFormatX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .SRA => |i| (base.InstrFormatX32{ .r = .{ .funct7 = 0b0100000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .OR => |i| (base.InstrFormatX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b110, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .AND => |i| (base.InstrFormatX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b111, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .FENCE => |i| (base.InstrFormatX32{ .f = .{ .fm = i.fm, .pred = i.pred, .succ = i.succ, .rs1 = i.rs1.to_u5(), .func3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b0001111 } }).to_varinstr(),
            .FENCE_I => (base.InstrFormatX32{ .f = .{ .fm = 0, .pred = .{}, .succ = .{}, .rs1 = 0, .func3 = 0b001, .rd = 0, .opcode = 0b0001111 } }).to_varinstr(),
            .ECALL => (base.InstrFormatX32{ .i = .{ .imm_11_0 = 0, .rs1 = 0, .funct3 = 0, .rd = 0, .opcode = 0b1110011 } }).to_varinstr(),
            .EBREAK => (base.InstrFormatX32{ .i = .{ .imm_11_0 = 1, .rs1 = 0, .funct3 = 0, .rd = 0, .opcode = 0b1110011 } }).to_varinstr(),
            .URET => (base.InstrFormatX32{ .i = .{ .imm_11_0 = 2, .rs1 = 0, .funct3 = 0, .rd = 0, .opcode = 0b1110011 } }).to_varinstr(),
            .SRET => (base.InstrFormatX32{ .i = .{ .imm_11_0 = 2 + (1 << 8), .rs1 = 0, .funct3 = 0, .rd = 0, .opcode = 0b1110011 } }).to_varinstr(),
            .MRET => (base.InstrFormatX32{ .i = .{ .imm_11_0 = 2 + (3 << 8), .rs1 = 0, .funct3 = 0, .rd = 0, .opcode = 0b1110011 } }).to_varinstr(),
        };

        return vari.to_memory(memory);
    }

    pub fn from_memory(instr: base.VarInstr) ?Self {
        return switch (instr) {
            .x32 => |x32| {
                const i = base.InstrFormatX32.from_u32(x32);

                return switch (i.opcode) {
                    0b0110111 => .{ .LUI = .{ .rd = IRf(i.u.rd), .imm = i.u.imm_31_12 } },
                    0b0010111 => .{ .AUIPC = .{ .rd = IRf(i.u.rd), .imm = i.u.imm_31_12 } },
                    0b1101111 => .{ .JAL = .{ .rd = IRf(i.j.rd), .imm = i.j.get_imm() } },
                    0b1100111 => switch (i.i.funct3) {
                        0b000 => .{ .JALR = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .imm = i.i.imm_11_0 } },
                        else => null,
                    },
                    0b1100011 => switch (i.b.funct3) {
                        0b000 => .{ .BEQ = .{ .rs1 = IRf(i.b.rs1), .rs2 = IRf(i.b.rs2), .offset = i.b.get_imm() } },
                        0b001 => .{ .BNE = .{ .rs1 = IRf(i.b.rs1), .rs2 = IRf(i.b.rs2), .offset = i.b.get_imm() } },
                        0b100 => .{ .BLT = .{ .rs1 = IRf(i.b.rs1), .rs2 = IRf(i.b.rs2), .offset = i.b.get_imm() } },
                        0b101 => .{ .BGE = .{ .rs1 = IRf(i.b.rs1), .rs2 = IRf(i.b.rs2), .offset = i.b.get_imm() } },
                        0b110 => .{ .BLTU = .{ .rs1 = IRf(i.b.rs1), .rs2 = IRf(i.b.rs2), .offset = i.b.get_imm() } },
                        0b111 => .{ .BGEU = .{ .rs1 = IRf(i.b.rs1), .rs2 = IRf(i.b.rs2), .offset = i.b.get_imm() } },
                        else => null,
                    },
                    0b0000011 => switch (i.i.funct3) {
                        0b000 => .{ .LB = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .offset = i.i.imm_11_0 } },
                        0b001 => .{ .LH = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .offset = i.i.imm_11_0 } },
                        0b010 => .{ .LW = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .offset = i.i.imm_11_0 } },
                        0b100 => .{ .LBU = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .offset = i.i.imm_11_0 } },
                        0b101 => .{ .LHU = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .offset = i.i.imm_11_0 } },
                        else => null,
                    },
                    0b0100011 => switch (i.s.funct3) {
                        0b000 => .{ .SB = .{ .rs1 = IRf(i.s.rs1), .rs2 = IRf(i.s.rs2), .offset = i.s.get_imm() } },
                        0b001 => .{ .SH = .{ .rs1 = IRf(i.s.rs1), .rs2 = IRf(i.s.rs2), .offset = i.s.get_imm() } },
                        0b010 => .{ .SW = .{ .rs1 = IRf(i.s.rs1), .rs2 = IRf(i.s.rs2), .offset = i.s.get_imm() } },
                        else => null,
                    },
                    0b0010011 => switch (i.i.funct3) {
                        0b000 => .{ .ADDI = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .imm = i.i.imm_11_0 } },
                        0b010 => .{ .SLTI = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .imm = i.i.imm_11_0 } },
                        0b011 => .{ .SLTIU = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .imm = i.i.imm_11_0 } },
                        0b100 => .{ .XORI = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .imm = i.i.imm_11_0 } },
                        0b110 => .{ .ORI = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .imm = i.i.imm_11_0 } },
                        0b111 => .{ .ANDI = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .imm = i.i.imm_11_0 } },
                        0b001 => .{ .SLLI = .{ .rd = IRf(i.i_1.rd), .rs1 = IRf(i.i_1.rs1), .shamt = @truncate(i.i_1.shamt) } },
                        0b101 => switch (i.i_1.op) {
                            0 => .{ .SRLI = .{ .rd = IRf(i.i_1.rd), .rs1 = IRf(i.i_1.rs1), .shamt = @truncate(i.i_1.shamt) } },
                            1 << 4 => .{ .SRAI = .{ .rd = IRf(i.i_1.rd), .rs1 = IRf(i.i_1.rs1), .shamt = @truncate(i.i_1.shamt) } },
                            else => null,
                        },
                    },
                    0b0110011 => switch (i.r.funct3) {
                        0b000 => switch (i.r.funct7) {
                            0 => .{ .ADD = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                            1 << 5 => .{ .SUB = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                            else => null,
                        },
                        0b001 => .{ .SLL = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                        0b010 => .{ .SLT = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                        0b011 => .{ .SLTU = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                        0b100 => .{ .XOR = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                        0b101 => switch (i.r.funct7) {
                            0 => .{ .SRL = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                            1 << 5 => .{ .SRA = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                            else => null,
                        },
                        0b110 => .{ .OR = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                        0b111 => .{ .AND = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                    },
                    0b0001111 => switch (i.f.func3) {
                        0b000 => .{ .FENCE = .{ .rd = IRf(i.f.rd), .rs1 = IRf(i.f.rs1), .succ = i.f.succ, .pred = i.f.pred, .fm = i.f.fm } },
                        0b001 => .FENCE_I,
                        else => null,
                    },
                    0b1110011 => switch (i.i.funct3) {
                        0b000 => switch (i.i.imm_11_0 & 31) {
                            0b00000 => .ECALL,
                            0b00001 => .EBREAK,
                            0b00010 => switch (i.i.imm_11_0 >> 8) {
                                0 => .URET,
                                1 => .SRET,
                                3 => .MRET,
                                else => null,
                            },
                            else => null,
                        },
                        else => null,
                    },
                    else => null,
                };
            },
            else => null,
        };
    }

    pub fn len(self: Self) usize {
        return switch (self) {
            .LUI, .AUIPC, .JAL, .JALR, .BEQ, .BNE, .BLT, .BGE, .BLTU, .BGEU, .LB, .LH, .LW, .LBU, .LHU, .SB, .SH, .SW, .ADDI, .SLTI, .SLTIU, .XORI, .ORI, .ANDI, .SLLI, .SRLI, .SRAI, .ADD, .SUB, .SLL, .SLT, .SLTU, .XOR, .SRL, .SRA, .OR, .AND, .FENCE, .FENCE_I, .ECALL, .EBREAK, .URET, .SRET, .MRET => 4,
        };
    }

    pub fn write(self: Self, writer: std.io.AnyWriter) !void {
        return switch (self) {
            .LUI => |i| writer.print("LUI {s}, 0x{x}\n", .{ i.rd.name(), i.imm }),
            .AUIPC => |i| writer.print("AUIPC {s}, 0x{x}\n", .{ i.rd.name(), i.imm }),
            .JAL => |i| writer.print("JAL {s}, 0x{x}\n", .{ i.rd.name(), i.imm }),
            .JALR => |i| writer.print("JALR {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.imm }),
            .BEQ => |i| writer.print("BEQ {s}, {s}, 0x{x}\n", .{ i.rs1.name(), i.rs2.name(), @as(u12, @bitCast(i.offset)) }),
            .BNE => |i| writer.print("BNE {s}, {s}, 0x{x}\n", .{ i.rs1.name(), i.rs2.name(), @as(u12, @bitCast(i.offset)) }),
            .BLT => |i| writer.print("BLT {s}, {s}, 0x{x}\n", .{ i.rs1.name(), i.rs2.name(), @as(u12, @bitCast(i.offset)) }),
            .BGE => |i| writer.print("BGE {s}, {s}, 0x{x}\n", .{ i.rs1.name(), i.rs2.name(), @as(u12, @bitCast(i.offset)) }),
            .BLTU => |i| writer.print("BLTU {s}, {s}, 0x{x}\n", .{ i.rs1.name(), i.rs2.name(), @as(u12, @bitCast(i.offset)) }),
            .BGEU => |i| writer.print("BGEU {s}, {s}, 0x{x}\n", .{ i.rs1.name(), i.rs2.name(), @as(u12, @bitCast(i.offset)) }),
            .LB => |i| writer.print("LB {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), @as(u12, @bitCast(i.offset)) }),
            .LH => |i| writer.print("LH {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), @as(u12, @bitCast(i.offset)) }),
            .LW => |i| writer.print("LW {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), @as(u12, @bitCast(i.offset)) }),
            .LBU => |i| writer.print("LBU {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), @as(u12, @bitCast(i.offset)) }),
            .LHU => |i| writer.print("LHU {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), @as(u12, @bitCast(i.offset)) }),
            .SB => |i| writer.print("SB {s}, {s}, 0x{x}\n", .{ i.rs1.name(), i.rs2.name(), @as(u12, @bitCast(i.offset)) }),
            .SH => |i| writer.print("SH {s}, {s}, 0x{x}\n", .{ i.rs1.name(), i.rs2.name(), @as(u12, @bitCast(i.offset)) }),
            .SW => |i| writer.print("SW {s}, {s}, 0x{x}\n", .{ i.rs1.name(), i.rs2.name(), @as(u12, @bitCast(i.offset)) }),
            .ADDI => |i| writer.print("ADDI {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.imm }),
            .SLTI => |i| writer.print("SLTI {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.imm }),
            .SLTIU => |i| writer.print("SLTIU {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.imm }),
            .XORI => |i| writer.print("XORI {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.imm }),
            .ORI => |i| writer.print("ORI {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.imm }),
            .ANDI => |i| writer.print("ANDI {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.imm }),
            .SLLI => |i| writer.print("SLLI {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.shamt }),
            .SRLI => |i| writer.print("SRLI {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.shamt }),
            .SRAI => |i| writer.print("SRAI {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.shamt }),
            .ADD => |i| writer.print("ADD {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .SUB => |i| writer.print("SUB {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .SLL => |i| writer.print("SLL {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .SLT => |i| writer.print("SLT {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .SLTU => |i| writer.print("SLTU {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .XOR => |i| writer.print("XOR {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .SRL => |i| writer.print("SRL {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .SRA => |i| writer.print("SRA {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .OR => |i| writer.print("OR {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .AND => |i| writer.print("AND {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .FENCE => |i| writer.print("FENCE {s}, {s}, {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.succ.name(), i.pred.name(), i.fm }),
            .FENCE_I => writer.print("FENCE.I\n", .{}),
            .ECALL => writer.print("ECALL\n", .{}),
            .EBREAK => writer.print("EBREAK\n", .{}),
            .URET => writer.print("URET\n", .{}),
            .SRET => writer.print("SRET\n", .{}),
            .MRET => writer.print("MRET\n", .{}),
        };
    }

    pub fn used_grs(self: Self) [3]IR {
        return switch (self) {
            .LUI => |i| .{ i.rd, IR.ZERO, IR.ZERO },
            .AUIPC => |i| .{ i.rd, IR.ZERO, IR.ZERO },
            .JAL => |i| .{ i.rd, IR.ZERO, IR.ZERO },
            .JALR => |i| .{ i.rd, i.rs1, IR.ZERO },
            .BEQ => |i| .{ i.rs1, i.rs2, IR.ZERO },
            .BNE => |i| .{ i.rs1, i.rs2, IR.ZERO },
            .BLT => |i| .{ i.rs1, i.rs2, IR.ZERO },
            .BGE => |i| .{ i.rs1, i.rs2, IR.ZERO },
            .BLTU => |i| .{ i.rs1, i.rs2, IR.ZERO },
            .BGEU => |i| .{ i.rs1, i.rs2, IR.ZERO },
            .LB => |i| .{ i.rd, i.rs1, IR.ZERO },
            .LH => |i| .{ i.rd, i.rs1, IR.ZERO },
            .LW => |i| .{ i.rd, i.rs1, IR.ZERO },
            .LBU => |i| .{ i.rd, i.rs1, IR.ZERO },
            .LHU => |i| .{ i.rd, i.rs1, IR.ZERO },
            .SB => |i| .{ i.rs1, i.rs2, IR.ZERO },
            .SH => |i| .{ i.rs1, i.rs2, IR.ZERO },
            .SW => |i| .{ i.rs1, i.rs2, IR.ZERO },
            .ADDI => |i| .{ i.rd, i.rs1, IR.ZERO },
            .SLTI => |i| .{ i.rd, i.rs1, IR.ZERO },
            .SLTIU => |i| .{ i.rd, i.rs1, IR.ZERO },
            .XORI => |i| .{ i.rd, i.rs1, IR.ZERO },
            .ORI => |i| .{ i.rd, i.rs1, IR.ZERO },
            .ANDI => |i| .{ i.rd, i.rs1, IR.ZERO },
            .SLLI => |i| .{ i.rd, i.rs1, IR.ZERO },
            .SRLI => |i| .{ i.rd, i.rs1, IR.ZERO },
            .SRAI => |i| .{ i.rd, i.rs1, IR.ZERO },
            .ADD => |i| .{ i.rd, i.rs1, i.rs2 },
            .SUB => |i| .{ i.rd, i.rs1, i.rs2 },
            .SLL => |i| .{ i.rd, i.rs1, i.rs2 },
            .SLT => |i| .{ i.rd, i.rs1, i.rs2 },
            .SLTU => |i| .{ i.rd, i.rs1, i.rs2 },
            .XOR => |i| .{ i.rd, i.rs1, i.rs2 },
            .SRL => |i| .{ i.rd, i.rs1, i.rs2 },
            .SRA => |i| .{ i.rd, i.rs1, i.rs2 },
            .OR => |i| .{ i.rd, i.rs1, i.rs2 },
            .AND => |i| .{ i.rd, i.rs1, i.rs2 },
            .FENCE => |i| .{ i.rd, i.rs1, IR.ZERO },
            .FENCE_I => .{ IR.ZERO, IR.ZERO, IR.ZERO },
            .ECALL => .{ IR.ZERO, IR.ZERO, IR.ZERO },
            .EBREAK => .{ IR.ZERO, IR.ZERO, IR.ZERO },
            .URET => .{ IR.ZERO, IR.ZERO, IR.ZERO },
            .SRET => .{ IR.ZERO, IR.ZERO, IR.ZERO },
            .MRET => .{ IR.ZERO, IR.ZERO, IR.ZERO },
        };
    }

    pub fn lui(rd: IR, imm: u20) Self {
        return Self{ .LUI = .{ .rd = rd, .imm = imm } };
    }
    pub fn auipc(rd: IR, imm: u20) Self {
        return Self{ .AUIPC = .{ .rd = rd, .imm = imm } };
    }
    pub fn jal(rd: IR, imm: i20) Self {
        return Self{ .JAL = .{ .rd = rd, .imm = imm } };
    }
    pub fn jalr(rd: IR, rs1: IR, imm: i12) Self {
        return Self{ .JALR = .{ .rd = rd, .rs1 = rs1, .imm = imm } };
    }
    pub fn beq(rs1: IR, rs2: IR, offset: i12) Self {
        return Self{ .BEQ = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn bne(rs1: IR, rs2: IR, offset: i12) Self {
        return Self{ .BNE = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn blt(rs1: IR, rs2: IR, offset: i12) Self {
        return Self{ .BLT = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn bge(rs1: IR, rs2: IR, offset: i12) Self {
        return Self{ .BGE = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn bltu(rs1: IR, rs2: IR, offset: i12) Self {
        return Self{ .BLTU = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn bgeu(rs1: IR, rs2: IR, offset: i12) Self {
        return Self{ .BGEU = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn lb(rd: IR, rs1: IR, offset: i12) Self {
        return Self{ .LB = .{ .rd = rd, .rs1 = rs1, .offset = offset } };
    }
    pub fn lh(rd: IR, rs1: IR, offset: i12) Self {
        return Self{ .LH = .{ .rd = rd, .rs1 = rs1, .offset = offset } };
    }
    pub fn lw(rd: IR, rs1: IR, offset: i12) Self {
        return Self{ .LW = .{ .rd = rd, .rs1 = rs1, .offset = offset } };
    }
    pub fn lbu(rd: IR, rs1: IR, offset: i12) Self {
        return Self{ .LBU = .{ .rd = rd, .rs1 = rs1, .offset = offset } };
    }
    pub fn lhu(rd: IR, rs1: IR, offset: i12) Self {
        return Self{ .LHU = .{ .rd = rd, .rs1 = rs1, .offset = offset } };
    }
    pub fn sb(rs1: IR, rs2: IR, offset: i12) Self {
        return Self{ .SB = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn sh(rs1: IR, rs2: IR, offset: i12) Self {
        return Self{ .SH = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn sw(rs1: IR, rs2: IR, offset: i12) Self {
        return Self{ .SW = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn addi(rd: IR, rs1: IR, imm: i12) Self {
        return Self{ .ADDI = .{ .rd = rd, .rs1 = rs1, .imm = imm } };
    }
    pub fn slti(rd: IR, rs1: IR, imm: i12) Self {
        return Self{ .SLTI = .{ .rd = rd, .rs1 = rs1, .imm = imm } };
    }
    pub fn sltiu(rd: IR, rs1: IR, imm: i12) Self {
        return Self{ .SLTIU = .{ .rd = rd, .rs1 = rs1, .imm = imm } };
    }
    pub fn xori(rd: IR, rs1: IR, imm: i12) Self {
        return Self{ .XORI = .{ .rd = rd, .rs1 = rs1, .imm = imm } };
    }
    pub fn ori(rd: IR, rs1: IR, imm: i12) Self {
        return Self{ .ORI = .{ .rd = rd, .rs1 = rs1, .imm = imm } };
    }
    pub fn andi(rd: IR, rs1: IR, imm: i12) Self {
        return Self{ .ANDI = .{ .rd = rd, .rs1 = rs1, .imm = imm } };
    }
    pub fn slli(rd: IR, rs1: IR, shamt: u5) Self {
        return Self{ .SLLI = .{ .rd = rd, .rs1 = rs1, .shamt = shamt } };
    }
    pub fn srli(rd: IR, rs1: IR, shamt: u5) Self {
        return Self{ .SRLI = .{ .rd = rd, .rs1 = rs1, .shamt = shamt } };
    }
    pub fn srai(rd: IR, rs1: IR, shamt: u5) Self {
        return Self{ .SRAI = .{ .rd = rd, .rs1 = rs1, .shamt = shamt } };
    }
    pub fn add(rd: IR, rs1: IR, rs2: IR) Self {
        return Self{ .ADD = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn sub(rd: IR, rs1: IR, rs2: IR) Self {
        return Self{ .SUB = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn sll(rd: IR, rs1: IR, rs2: IR) Self {
        return Self{ .SLL = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn slt(rd: IR, rs1: IR, rs2: IR) Self {
        return Self{ .SLT = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn sltu(rd: IR, rs1: IR, rs2: IR) Self {
        return Self{ .SLTU = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn xor(rd: IR, rs1: IR, rs2: IR) Self {
        return Self{ .XOR = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn srl(rd: IR, rs1: IR, rs2: IR) Self {
        return Self{ .SRL = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn sra(rd: IR, rs1: IR, rs2: IR) Self {
        return Self{ .SRA = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn _or(rd: IR, rs1: IR, rs2: IR) Self {
        return Self{ .ORI = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn _and(rd: IR, rs1: IR, rs2: IR) Self {
        return Self{ .AND = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn fence(rd: IR, rs1: IR, succ: base.FFlags, pred: base.FFlags, fm: u4) Self {
        return Self{ .FENCE = .{ .rd = rd, .rs1 = rs1, .succ = succ, .pred = pred, .fm = fm } };
    }
    pub fn ecall() Self {
        return .ECALL;
    }
    pub fn ebreak() Self {
        return .EBREAK;
    }
    pub fn uret() Self {
        return .URET;
    }
    pub fn sret() Self {
        return .SRET;
    }
    pub fn mret() Self {
        return .MRET;
    }
};

pub const RV64I = union(enum) {
    LWU: struct { rd: IR, rs1: IR, offset: i12 },
    LD: struct { rd: IR, rs1: IR, offset: i12 },
    SD: struct { rs1: IR, rs2: IR, offset: i12 },
    SLLI: struct { rd: IR, rs1: IR, shamt: u6 },
    SRLI: struct { rd: IR, rs1: IR, shamt: u6 },
    SRAI: struct { rd: IR, rs1: IR, shamt: u6 },
    ADDIW: struct { rd: IR, rs1: IR, imm: i12 },
    SLLIW: struct { rd: IR, rs1: IR, shamt: u6 },
    SRLIW: struct { rd: IR, rs1: IR, shamt: u6 },
    SRAIW: struct { rd: IR, rs1: IR, shamt: u6 },
    ADDW: struct { rd: IR, rs1: IR, rs2: IR },
    SUBW: struct { rd: IR, rs1: IR, rs2: IR },
    SLLW: struct { rd: IR, rs1: IR, rs2: IR },
    SRLW: struct { rd: IR, rs1: IR, rs2: IR },
    SRAW: struct { rd: IR, rs1: IR, rs2: IR },

    const Self = @This();

    pub fn to_memory(self: Self, memory: []u8) error{OutOfSpace}!usize {
        const vari = switch (self) {
            .LWU => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = i.offset, .rs1 = i.rs1.to_u5(), .funct3 = 0b110, .rd = i.rd.to_u5(), .opcode = 0b0000011 } }).to_varinstr(),
            .LD => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = i.offset, .rs1 = i.rs1.to_u5(), .funct3 = 0b011, .rd = i.rd.to_u5(), .opcode = 0b0000011 } }).to_varinstr(),
            .SD => |i| (base.InstrFormatX32{ .s = .{ .imm_11_5 = @truncate(cast(u12, i.offset) >> 5), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b011, .imm_4_0 = @truncate(cast(u12, i.offset)), .opcode = 0b0100011 } }).to_varinstr(),
            .SLLI => |i| (base.InstrFormatX32{ .i_1 = .{ .op = 0b000000, .shamt = i.shamt, .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .SRLI => |i| (base.InstrFormatX32{ .i_1 = .{ .op = 0b000000, .shamt = i.shamt, .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .SRAI => |i| (base.InstrFormatX32{ .i_1 = .{ .op = 0b010000, .shamt = i.shamt, .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .ADDIW => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = i.imm, .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b0011011 } }).to_varinstr(),
            .SLLIW => |i| (base.InstrFormatX32{ .i_1 = .{ .op = 0b000000, .shamt = i.shamt, .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .rd = i.rd.to_u5(), .opcode = 0b0011011 } }).to_varinstr(),
            .SRLIW => |i| (base.InstrFormatX32{ .i_1 = .{ .op = 0b000000, .shamt = i.shamt, .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0011011 } }).to_varinstr(),
            .SRAIW => |i| (base.InstrFormatX32{ .i_1 = .{ .op = 0b010000, .shamt = i.shamt, .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0011011 } }).to_varinstr(),
            .ADDW => |i| (base.InstrFormatX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b0111011 } }).to_varinstr(),
            .SUBW => |i| (base.InstrFormatX32{ .r = .{ .funct7 = 0b0100000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b0111011 } }).to_varinstr(),
            .SLLW => |i| (base.InstrFormatX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .rd = i.rd.to_u5(), .opcode = 0b0111011 } }).to_varinstr(),
            .SRLW => |i| (base.InstrFormatX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0111011 } }).to_varinstr(),
            .SRAW => |i| (base.InstrFormatX32{ .r = .{ .funct7 = 0b0100000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0111011 } }).to_varinstr(),
        };

        return vari.to_memory(memory);
    }

    pub fn from_memory(instr: base.VarInstr) ?Self {
        return switch (instr) {
            .x32 => |x32| {
                const i = base.InstrFormatX32.from_u32(x32);
                return switch (i.opcode) {
                    0b0000011 => switch (i.i.funct3) {
                        0b110 => .{ .LWU = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .offset = i.i.imm_11_0 } },
                        0b011 => .{ .LD = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .offset = i.i.imm_11_0 } },
                        else => null,
                    },
                    0b0100011 => switch (i.s.funct3) {
                        0b011 => .{ .SD = .{ .rs1 = IRf(i.s.rs1), .rs2 = IRf(i.s.rs2), .offset = i.s.get_imm() } },
                        else => null,
                    },
                    0b0010011 => switch (i.i.funct3) {
                        0b001 => .{ .SLLI = .{ .rd = IRf(i.i_1.rd), .rs1 = IRf(i.i_1.rs1), .shamt = i.i_1.shamt } },
                        0b101 => switch (i.i_1.op) {
                            0 => .{ .SRLI = .{ .rd = IRf(i.i_1.rd), .rs1 = IRf(i.i_1.rs1), .shamt = i.i_1.shamt } },
                            1 << 4 => .{ .SRAI = .{ .rd = IRf(i.i_1.rd), .rs1 = IRf(i.i_1.rs1), .shamt = i.i_1.shamt } },
                            else => null,
                        },
                        else => null,
                    },
                    0b0011011 => switch (i.i.funct3) {
                        0b000 => .{ .ADDIW = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .imm = i.i.imm_11_0 } },
                        0b001 => .{ .SLLIW = .{ .rd = IRf(i.i_1.rd), .rs1 = IRf(i.i_1.rs1), .shamt = i.i_1.shamt } },
                        0b101 => switch (i.i_1.op) {
                            0 => .{ .SRLIW = .{ .rd = IRf(i.i_1.rd), .rs1 = IRf(i.i_1.rs1), .shamt = i.i_1.shamt } },
                            1 << 4 => .{ .SRAIW = .{ .rd = IRf(i.i_1.rd), .rs1 = IRf(i.i_1.rs1), .shamt = i.i_1.shamt } },
                            else => null,
                        },
                        else => null,
                    },
                    0b0111011 => switch (i.r.funct3) {
                        0b000 => switch (i.r.funct7) {
                            0 => .{ .ADDW = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                            1 << 5 => .{ .SUBW = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                            else => null,
                        },
                        0b001 => .{ .SLLW = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                        0b101 => switch (i.r.funct7) {
                            0 => .{ .SRLW = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                            1 << 5 => .{ .SRAW = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                            else => null,
                        },
                        else => null,
                    },
                    else => null,
                };
            },
            else => null,
        };
    }

    pub fn len(self: Self) usize {
        return switch (self) {
            .LWU, .LD, .SD, .SLLI, .SRLI, .SRAI, .ADDIW, .SLLIW, .SRLIW, .SRAIW, .ADDW, .SUBW, .SLLW, .SRLW, .SRAW => 4,
        };
    }

    pub fn write(self: Self, writer: std.io.AnyWriter) !void {
        return switch (self) {
            .LWU => |i| writer.print("LWU {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), @as(u12, @bitCast(i.offset)) }),
            .LD => |i| writer.print("LD {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), @as(u12, @bitCast(i.offset)) }),
            .SD => |i| writer.print("SD {s}, {s}, 0x{x}\n", .{ i.rs1.name(), i.rs2.name(), @as(u12, @bitCast(i.offset)) }),
            .SLLI => |i| writer.print("SLLI {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.shamt }),
            .SRLI => |i| writer.print("SRLI {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.shamt }),
            .SRAI => |i| writer.print("SRAI {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.shamt }),
            .ADDIW => |i| writer.print("ADDIW {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.imm }),
            .SLLIW => |i| writer.print("SLLIW {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.shamt }),
            .SRLIW => |i| writer.print("SRLIW {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.shamt }),
            .SRAIW => |i| writer.print("SRAIW {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.shamt }),
            .ADDW => |i| writer.print("ADDW {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .SUBW => |i| writer.print("SUBW {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .SLLW => |i| writer.print("SLLW {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .SRLW => |i| writer.print("SRL {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .SRAW => |i| writer.print("SRA {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
        };
    }

    pub fn used_grs(self: Self) [3]IR {
        return switch (self) {
            .LWU => |i| .{ i.rd, i.rs1, IR.ZERO },
            .LD => |i| .{ i.rd, i.rs1, IR.ZERO },
            .SD => |i| .{ i.rs1, i.rs2, IR.ZERO },
            .SLLI => |i| .{ i.rd, i.rs1, IR.ZERO },
            .SRLI => |i| .{ i.rd, i.rs1, IR.ZERO },
            .SRAI => |i| .{ i.rd, i.rs1, IR.ZERO },
            .ADDIW => |i| .{ i.rd, i.rs1, IR.ZERO },
            .SLLIW => |i| .{ i.rd, i.rs1, IR.ZERO },
            .SRLIW => |i| .{ i.rd, i.rs1, IR.ZERO },
            .SRAIW => |i| .{ i.rd, i.rs1, IR.ZERO },
            .ADDW => |i| .{ i.rd, i.rs1, i.rs2 },
            .SUBW => |i| .{ i.rd, i.rs1, i.rs2 },
            .SLLW => |i| .{ i.rd, i.rs1, i.rs2 },
            .SRLW => |i| .{ i.rd, i.rs1, i.rs2 },
            .SRAW => |i| .{ i.rd, i.rs1, i.rs2 },
        };
    }

    pub fn lwu(rd: IR, rs1: IR, offset: i12) Self {
        return Self{ .LWU = .{ .rd = rd, .rs1 = rs1, .offset = offset } };
    }
    pub fn ld(rd: IR, rs1: IR, offset: i12) Self {
        return Self{ .LD = .{ .rd = rd, .rs1 = rs1, .offset = offset } };
    }
    pub fn sd(rs1: IR, rs2: IR, offset: i12) Self {
        return Self{ .SD = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn slli(rd: IR, rs1: IR, shamt: u6) Self {
        return Self{ .SLLI = .{ .rd = rd, .rs1 = rs1, .shamt = shamt } };
    }
    pub fn srli(rd: IR, rs1: IR, shamt: u6) Self {
        return Self{ .SRLI = .{ .rd = rd, .rs1 = rs1, .shamt = shamt } };
    }
    pub fn srai(rd: IR, rs1: IR, shamt: u6) Self {
        return Self{ .SRAI = .{ .rd = rd, .rs1 = rs1, .shamt = shamt } };
    }
};

pub const RV32M = union(enum) {
    MUL: struct { rd: IR, rs1: IR, rs2: IR },
    MULH: struct { rd: IR, rs1: IR, rs2: IR },
    MULHSU: struct { rd: IR, rs1: IR, rs2: IR },
    MULHU: struct { rd: IR, rs1: IR, rs2: IR },
    DIV: struct { rd: IR, rs1: IR, rs2: IR },
    DIVU: struct { rd: IR, rs1: IR, rs2: IR },
    REM: struct { rd: IR, rs1: IR, rs2: IR },
    REMU: struct { rd: IR, rs1: IR, rs2: IR },

    const Self = @This();

    pub fn to_memory(self: Self, memory: []u8) error{OutOfSpace}!usize {
        const vari = switch (self) {
            .MUL => |i| (IF32{ .r = .{ .funct7 = 0b0000001, .funct3 = 0b000, .opcode = 0b0110011, .rd = i.rd.to_u5(), .rs1 = i.rs1.to_u5(), .rs2 = i.rs2.to_u5() } }),
            .MULH => |i| (IF32{ .r = .{ .funct7 = 0b0000001, .funct3 = 0b001, .opcode = 0b0110011, .rd = i.rd.to_u5(), .rs1 = i.rs1.to_u5(), .rs2 = i.rs2.to_u5() } }),
            .MULHSU => |i| (IF32{ .r = .{ .funct7 = 0b0000001, .funct3 = 0b010, .opcode = 0b0110011, .rd = i.rd.to_u5(), .rs1 = i.rs1.to_u5(), .rs2 = i.rs2.to_u5() } }),
            .MULHU => |i| (IF32{ .r = .{ .funct7 = 0b0000001, .funct3 = 0b011, .opcode = 0b0110011, .rd = i.rd.to_u5(), .rs1 = i.rs1.to_u5(), .rs2 = i.rs2.to_u5() } }),
            .DIV => |i| (IF32{ .r = .{ .funct7 = 0b0000001, .funct3 = 0b100, .opcode = 0b0110011, .rd = i.rd.to_u5(), .rs1 = i.rs1.to_u5(), .rs2 = i.rs2.to_u5() } }),
            .DIVU => |i| (IF32{ .r = .{ .funct7 = 0b0000001, .funct3 = 0b101, .opcode = 0b0110011, .rd = i.rd.to_u5(), .rs1 = i.rs1.to_u5(), .rs2 = i.rs2.to_u5() } }),
            .REM => |i| (IF32{ .r = .{ .funct7 = 0b0000001, .funct3 = 0b110, .opcode = 0b0110011, .rd = i.rd.to_u5(), .rs1 = i.rs1.to_u5(), .rs2 = i.rs2.to_u5() } }),
            .REMU => |i| (IF32{ .r = .{ .funct7 = 0b0000001, .funct3 = 0b111, .opcode = 0b0110011, .rd = i.rd.to_u5(), .rs1 = i.rs1.to_u5(), .rs2 = i.rs2.to_u5() } }),
        };
        return vari.to_varinstr().to_memory(memory);
    }

    pub fn from_memory(instr: base.VarInstr) ?Self {
        return switch (instr) {
            .x32 => |x32| {
                const i = base.InstrFormatX32.from_u32(x32);

                if (i.r.opcode != 0b0110011 or i.r.funct7 != 0b0000001) {
                    return null;
                }

                return switch (i.r.funct3) {
                    0b000 => Self{ .MUL = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                    0b001 => Self{ .MULH = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                    0b010 => Self{ .MULHSU = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                    0b011 => Self{ .MULHU = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                    0b100 => Self{ .DIV = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                    0b101 => Self{ .DIVU = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                    0b110 => Self{ .REM = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                    0b111 => Self{ .REMU = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                };
            },
            else => null,
        };
    }

    pub fn len(self: Self) usize {
        _ = self;
        return 4;
    }

    pub fn write(self: Self, writer: std.io.AnyWriter) !void {
        return switch (self) {
            .MUL => |i| writer.print("MUL {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .MULH => |i| writer.print("MULH {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .MULHSU => |i| writer.print("MULHSU {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .MULHU => |i| writer.print("MULHU {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .DIV => |i| writer.print("DIV {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .DIVU => |i| writer.print("DIVU {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .REM => |i| writer.print("REM {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .REMU => |i| writer.print("REMU {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
        };
    }

    pub fn used_grs(self: Self) [3]IR {
        return switch (self) {
            .MUL => |i| .{ i.rd, i.rs1, i.rs2 },
            .MULH => |i| .{ i.rd, i.rs1, i.rs2 },
            .MULHSU => |i| .{ i.rd, i.rs1, i.rs2 },
            .MULHU => |i| .{ i.rd, i.rs1, i.rs2 },
            .DIV => |i| .{ i.rd, i.rs1, i.rs2 },
            .DIVU => |i| .{ i.rd, i.rs1, i.rs2 },
            .REM => |i| .{ i.rd, i.rs1, i.rs2 },
            .REMU => |i| .{ i.rd, i.rs1, i.rs2 },
        };
    }
};

pub const RV64M = union(enum) {
    MULW: struct { rd: IR, rs1: IR, rs2: IR },
    DIVW: struct { rd: IR, rs1: IR, rs2: IR },
    DIVUW: struct { rd: IR, rs1: IR, rs2: IR },
    REMW: struct { rd: IR, rs1: IR, rs2: IR },
    REMUW: struct { rd: IR, rs1: IR, rs2: IR },

    const Self = @This();

    pub fn to_memory(self: Self, memory: []u8) error{OutOfSpace}!usize {
        const vari = switch (self) {
            .MULW => |i| (IF32{ .r = .{ .funct7 = 0b0000001, .funct3 = 0b000, .opcode = 0b0111011, .rd = i.rd.to_u5(), .rs1 = i.rs1.to_u5(), .rs2 = i.rs2.to_u5() } }),
            .DIVW => |i| (IF32{ .r = .{ .funct7 = 0b0000001, .funct3 = 0b100, .opcode = 0b0111011, .rd = i.rd.to_u5(), .rs1 = i.rs1.to_u5(), .rs2 = i.rs2.to_u5() } }),
            .DIVUW => |i| (IF32{ .r = .{ .funct7 = 0b0000001, .funct3 = 0b101, .opcode = 0b0111011, .rd = i.rd.to_u5(), .rs1 = i.rs1.to_u5(), .rs2 = i.rs2.to_u5() } }),
            .REMW => |i| (IF32{ .r = .{ .funct7 = 0b0000001, .funct3 = 0b110, .opcode = 0b0111011, .rd = i.rd.to_u5(), .rs1 = i.rs1.to_u5(), .rs2 = i.rs2.to_u5() } }),
            .REMUW => |i| (IF32{ .r = .{ .funct7 = 0b0000001, .funct3 = 0b111, .opcode = 0b0111011, .rd = i.rd.to_u5(), .rs1 = i.rs1.to_u5(), .rs2 = i.rs2.to_u5() } }),
        };
        return vari.to_varinstr().to_memory(memory);
    }

    pub fn from_memory(instr: base.VarInstr) ?Self {
        return switch (instr) {
            .x32 => |x32| {
                const i = base.InstrFormatX32.from_u32(x32);

                if (i.r.opcode != 0b0111011 or i.r.funct7 != 0b0000001) {
                    return null;
                }

                return switch (i.r.funct3) {
                    0b000 => Self{ .MULW = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                    0b100 => Self{ .DIVW = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                    0b101 => Self{ .DIVUW = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                    0b110 => Self{ .REMW = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                    0b111 => Self{ .REMUW = .{ .rd = IRf(i.r.rd), .rs1 = IRf(i.r.rs1), .rs2 = IRf(i.r.rs2) } },
                    else => null,
                };
            },
            else => null,
        };
    }

    pub fn len(self: Self) usize {
        _ = self;
        return 4;
    }

    pub fn write(self: Self, writer: std.io.AnyWriter) !void {
        return switch (self) {
            .MULW => |i| writer.print("MULW {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .DIVW => |i| writer.print("DIVW {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .DIVUW => |i| writer.print("DIVUW {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .REMW => |i| writer.print("REMW {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .REMUW => |i| writer.print("REMUW {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
        };
    }

    pub fn used_grs(self: Self) [3]IR {
        return switch (self) {
            .MULW => |i| .{ i.rd, i.rs1, i.rs2 },
            .DIVW => |i| .{ i.rd, i.rs1, i.rs2 },
            .DIVUW => |i| .{ i.rd, i.rs1, i.rs2 },
            .REMW => |i| .{ i.rd, i.rs1, i.rs2 },
            .REMUW => |i| .{ i.rd, i.rs1, i.rs2 },
        };
    }
};

pub const Ziscr = union(enum) {
    CSRRW: struct { rd: IR, rs1: IR, csr: u12 },
    CSRRS: struct { rd: IR, rs1: IR, csr: u12 },
    CSRRC: struct { rd: IR, rs1: IR, csr: u12 },
    CSRRWI: struct { rd: IR, uimm: u5, csr: u12 },
    CSRRSI: struct { rd: IR, uimm: u5, csr: u12 },
    CSRRCI: struct { rd: IR, uimm: u5, csr: u12 },

    const Self = @This();

    pub fn to_memory(self: Self, memory: []u8) error{OutOfSpace}!usize {
        const vari = switch (self) {
            .CSRRW => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = @bitCast(i.csr), .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .rd = i.rd.to_u5(), .opcode = 0b1110011 } }).to_varinstr(),
            .CSRRS => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = @bitCast(i.csr), .rs1 = i.rs1.to_u5(), .funct3 = 0b010, .rd = i.rd.to_u5(), .opcode = 0b1110011 } }).to_varinstr(),
            .CSRRC => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = @bitCast(i.csr), .rs1 = i.rs1.to_u5(), .funct3 = 0b011, .rd = i.rd.to_u5(), .opcode = 0b1110011 } }).to_varinstr(),
            .CSRRWI => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = @bitCast(i.csr), .rs1 = i.uimm, .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b1110011 } }).to_varinstr(),
            .CSRRSI => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = @bitCast(i.csr), .rs1 = i.uimm, .funct3 = 0b110, .rd = i.rd.to_u5(), .opcode = 0b1110011 } }).to_varinstr(),
            .CSRRCI => |i| (base.InstrFormatX32{ .i = .{ .imm_11_0 = @bitCast(i.csr), .rs1 = i.uimm, .funct3 = 0b111, .rd = i.rd.to_u5(), .opcode = 0b1110011 } }).to_varinstr(),
        };

        return vari.to_memory(memory);
    }

    pub fn from_memory(instr: base.VarInstr) ?Self {
        return switch (instr) {
            .x32 => |x32| {
                const i = base.InstrFormatX32.from_u32(x32);

                return switch (i.opcode) {
                    0b1110011 => switch (i.i.funct3) {
                        0b001 => .{ .CSRRW = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .csr = @bitCast(i.i.imm_11_0) } },
                        0b010 => .{ .CSRRS = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .csr = @bitCast(i.i.imm_11_0) } },
                        0b011 => .{ .CSRRC = .{ .rd = IRf(i.i.rd), .rs1 = IRf(i.i.rs1), .csr = @bitCast(i.i.imm_11_0) } },
                        0b101 => .{ .CSRRWI = .{ .rd = IRf(i.i.rd), .uimm = i.i.rs1, .csr = @bitCast(i.i.imm_11_0) } },
                        0b110 => .{ .CSRRSI = .{ .rd = IRf(i.i.rd), .uimm = i.i.rs1, .csr = @bitCast(i.i.imm_11_0) } },
                        0b111 => .{ .CSRRCI = .{ .rd = IRf(i.i.rd), .uimm = i.i.rs1, .csr = @bitCast(i.i.imm_11_0) } },
                        else => null,
                    },
                    else => null,
                };
            },
            else => null,
        };
    }

    pub fn len(self: Self) usize {
        return switch (self) {
            .CSRRW, .CSRRS, .CSRRC, .CSRRWI, .CSRRSI, .CSRRCI => 4,
        };
    }

    pub fn write(self: Self, writer: std.io.AnyWriter) !void {
        return switch (self) {
            .CSRRW => |i| writer.print("CSRRW {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.csr }),
            .CSRRS => |i| writer.print("CSRRS {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.csr }),
            .CSRRC => |i| writer.print("CSRRC {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.csr }),
            .CSRRWI => |i| writer.print("CSRRWI {s}, 0x{x}, 0x{x}\n", .{ i.rd.name(), i.uimm, i.csr }),
            .CSRRSI => |i| writer.print("CSRRSI {s}, 0x{x}, 0x{x}\n", .{ i.rd.name(), i.uimm, i.csr }),
            .CSRRCI => |i| writer.print("CSRRCI {s}, 0x{x}, 0x{x}\n", .{ i.rd.name(), i.uimm, i.csr }),
        };
    }

    pub fn used_grs(self: Self) [3]IR {
        return switch (self) {
            .CSRRW => |i| .{ i.rd, i.rs1, IR.ZERO },
            .CSRRS => |i| .{ i.rd, i.rs1, IR.ZERO },
            .CSRRC => |i| .{ i.rd, i.rs1, IR.ZERO },
            .CSRRWI => |i| .{ i.rd, IR.ZERO, IR.ZERO },
            .CSRRSI => |i| .{ i.rd, IR.ZERO, IR.ZERO },
            .CSRRCI => |i| .{ i.rd, IR.ZERO, IR.ZERO },
        };
    }

    pub fn csrrw(rd: IR, rs1: IR, csr: u12) Self {
        return Self{ .CSRRW = .{ .rd = rd, .rs1 = rs1, .csr = csr } };
    }
    pub fn csrrs(rd: IR, rs1: IR, csr: u12) Self {
        return Self{ .CSRRS = .{ .rd = rd, .rs1 = rs1, .csr = csr } };
    }
    pub fn csrrc(rd: IR, rs1: IR, csr: u12) Self {
        return Self{ .CSRRC = .{ .rd = rd, .rs1 = rs1, .csr = csr } };
    }
    pub fn csrrwi(rd: IR, uimm: u5, csr: u12) Self {
        return Self{ .CSRRWI = .{ .rd = rd, .uimm = uimm, .csr = csr } };
    }
    pub fn csrrsi(rd: IR, uimm: u5, csr: u12) Self {
        return Self{ .CSRRSI = .{ .rd = rd, .uimm = uimm, .csr = csr } };
    }
    pub fn csrrci(rd: IR, uimm: u5, csr: u12) Self {
        return Self{ .CSRRCI = .{ .rd = rd, .uimm = uimm, .csr = csr } };
    }
};

pub const RV32C = union(enum) {
    C_LWSP: struct { rd: IR, uimm: u6 },
    C_FLWSP: struct { rd: IR, uimm: u6 },
    C_SWSP: struct { rs2: IR, uimm: u6 },
    C_FSWSP: struct { rs2: IR, uimm: u6 },
    C_LW: struct { rd: PIR, rs1: PIR, uimm: u5 },
    C_FLW: struct { rd: PIR, rs1: PIR, uimm: u5 },
    C_SW: struct { rs1: PIR, rs2: PIR, uimm: u5 },
    C_FSW: struct { rs1: PIR, rs2: PIR, uimm: u5 },
    C_J: struct { imm: i11 },
    C_JAL: struct { imm: i11 },
    C_JR: struct { rs1: IR },
    C_JALR: struct { rs1: IR },
    C_BEQZ: struct { rs1: PIR, imm: i8 },
    C_BNEZ: struct { rs1: PIR, imm: i8 },
    C_LI: struct { rd: IR, imm: i6 },
    C_LUI: struct { rd: IR, imm: i6 },
    C_ADDI: struct { rd: IR, imm: i6 },
    C_ADDI16SP: struct { imm: i6 },
    C_ADDI4SPN: struct { rd: PIR, uimm: u8 },
    C_SLLI: struct { rd: IR, shamt: u6 },
    C_SRLI: struct { rd: PIR, shamt: u6 },
    C_SRAI: struct { rd: PIR, shamt: u6 },
    C_ANDI: struct { rd: PIR, imm: i6 },
    C_MV: struct { rd: IR, rs2: IR },
    C_ADD: struct { rd: IR, rs2: IR },
    C_AND: struct { rd: PIR, rs2: PIR },
    C_OR: struct { rd: PIR, rs2: PIR },
    C_XOR: struct { rd: PIR, rs2: PIR },
    C_SUB: struct { rd: PIR, rs2: PIR },
    C_EBREAK,

    const Self = @This();

    pub fn to_memory(self: Self, memory: []u8) error{OutOfSpace}!usize {
        return switch (self) {
            .C_LWSP => |i| (IF16{ .ci = .{ .funct3 = 0b010, .op = 0b10, .imm_12 = t(u1, i.uimm >> 5), .rd = i.rd.to_u5(), .imm_2_6 = t(u5, i.uimm) } }).to_varinstr().to_memory(memory),
            .C_FLWSP => |i| (IF16{ .ci = .{ .funct3 = 0b011, .op = 0b10, .imm_12 = t(u1, i.uimm >> 5), .rd = i.rd.to_u5(), .imm_2_6 = t(u5, i.uimm) } }).to_varinstr().to_memory(memory),
            .C_SWSP => |i| (IF16{ .css = .{ .funct3 = 0b110, .op = 0b10, .imm = i.uimm, .rs2 = i.rs2.to_u5() } }).to_varinstr().to_memory(memory),
            .C_FSWSP => |i| (IF16{ .css = .{ .funct3 = 0b111, .op = 0b10, .imm = i.uimm, .rs2 = i.rs2.to_u5() } }).to_varinstr().to_memory(memory),
            .C_LW => |i| (IF16{ .cl = .{ .funct3 = 0b010, .op = 0b00, .imm2 = t(u3, i.uimm >> 2), .prs1 = i.rs1.to_u3(), .imm1 = t(u2, i.uimm), .prd = i.rd.to_u3() } }).to_varinstr().to_memory(memory),
            .C_FLW => |i| (IF16{ .cl = .{ .funct3 = 0b011, .op = 0b00, .imm2 = t(u3, i.uimm >> 2), .prs1 = i.rs1.to_u3(), .imm1 = t(u2, i.uimm), .prd = i.rd.to_u3() } }).to_varinstr().to_memory(memory),
            .C_SW => |i| (IF16{ .cs = .{ .funct3 = 0b110, .op = 0b00, .imm2 = t(u3, i.uimm >> 2), .imm1 = t(u2, i.uimm), .prs1 = i.rs1.to_u3(), .prs2 = i.rs2.to_u3() } }).to_varinstr().to_memory(memory),
            .C_FSW => |i| (IF16{ .cs = .{ .funct3 = 0b111, .op = 0b00, .imm2 = t(u3, i.uimm >> 2), .imm1 = t(u2, i.uimm), .prs1 = i.rs1.to_u3(), .prs2 = i.rs2.to_u3() } }).to_varinstr().to_memory(memory),
            .C_J => |i| (IF16{ .cj = .{ .funct3 = 0b101, .op = 0b01, .target = i.imm } }).to_varinstr().to_memory(memory),
            .C_JAL => |i| (IF16{ .cj = .{ .funct3 = 0b001, .op = 0b01, .target = i.imm } }).to_varinstr().to_memory(memory),
            .C_JR => |i| (IF16{ .cr = .{ .funct4 = 0b1000, .op = 0b10, .rs2 = i.rs1.to_u5(), .rd = 0 } }).to_varinstr().to_memory(memory),
            .C_JALR => |i| (IF16{ .cr = .{ .funct4 = 0b1001, .op = 0b10, .rs2 = i.rs1.to_u5(), .rd = 0 } }).to_varinstr().to_memory(memory),
            .C_BEQZ => |i| (IF16{ .cb = .{ .funct3 = 0b110, .op = 0b01, .offset2 = t(u3, cast(u8, i.imm) >> 5), .prd = i.rs1.to_u3(), .offset1 = t(u5, cast(u8, i.imm)) } }).to_varinstr().to_memory(memory),
            .C_BNEZ => |i| (IF16{ .cb = .{ .funct3 = 0b111, .op = 0b01, .offset2 = t(u3, cast(u8, i.imm) >> 5), .prd = i.rs1.to_u3(), .offset1 = t(u5, cast(u8, i.imm)) } }).to_varinstr().to_memory(memory),
            .C_LI => |i| (IF16{ .ci = .{ .funct3 = 0b010, .op = 0b01, .imm_12 = t(u1, cast(u6, i.imm) >> 5), .rd = i.rd.to_u5(), .imm_2_6 = t(u5, cast(u6, i.imm)) } }).to_varinstr().to_memory(memory),
            .C_LUI => |i| (IF16{ .ci = .{ .funct3 = 0b011, .op = 0b01, .imm_12 = t(u1, cast(u6, i.imm) >> 5), .rd = i.rd.to_u5(), .imm_2_6 = t(u5, cast(u6, i.imm)) } }).to_varinstr().to_memory(memory),
            .C_ADDI => |i| (IF16{ .ci = .{ .funct3 = 0b000, .op = 0b01, .imm_12 = t(u1, cast(u6, i.imm) >> 5), .rd = i.rd.to_u5(), .imm_2_6 = t(u5, cast(u6, i.imm)) } }).to_varinstr().to_memory(memory),
            .C_ADDI16SP => |i| (IF16{ .ci = .{ .funct3 = 0b011, .op = 0b01, .imm_12 = t(u1, cast(u6, i.imm) >> 5), .rd = 0b00010, .imm_2_6 = t(u5, cast(u6, i.imm)) } }).to_varinstr().to_memory(memory),
            .C_ADDI4SPN => |i| (IF16{ .ciw = .{ .funct3 = 0b000, .op = 0b00, .imm = i.uimm, .prd = i.rd.to_u3() } }).to_varinstr().to_memory(memory),
            .C_SLLI => |i| (IF16{ .ci = .{ .funct3 = 0b000, .op = 0b10, .imm_12 = t(u1, i.shamt >> 5), .rd = i.rd.to_u5(), .imm_2_6 = t(u5, i.shamt) } }).to_varinstr().to_memory(memory),
            .C_SRLI => |i| (IF16{ .cb = .{ .funct3 = 0b100, .op = 0b01, .offset2 = (s(u3, t(u1, i.shamt >> 5)) << 2) | 0b00, .prd = i.rd.to_u3(), .offset1 = t(u5, i.shamt) } }).to_varinstr().to_memory(memory),
            .C_SRAI => |i| (IF16{ .cb = .{ .funct3 = 0b100, .op = 0b01, .offset2 = (s(u3, t(u1, i.shamt >> 5)) << 2) | 0b01, .prd = i.rd.to_u3(), .offset1 = t(u5, i.shamt) } }).to_varinstr().to_memory(memory),
            .C_ANDI => |i| (IF16{ .cb = .{ .funct3 = 0b100, .op = 0b01, .offset2 = (s(u3, t(u1, cast(u6, i.imm) >> 5)) << 2) | 0b10, .prd = i.rd.to_u3(), .offset1 = t(u5, cast(u6, i.imm)) } }).to_varinstr().to_memory(memory),
            .C_MV => |i| (IF16{ .cr = .{ .funct4 = 0b1000, .op = 0b10, .rd = i.rd.to_u5(), .rs2 = i.rs2.to_u5() } }).to_varinstr().to_memory(memory),
            .C_ADD => |i| (IF16{ .cr = .{ .funct4 = 0b1001, .op = 0b10, .rd = i.rd.to_u5(), .rs2 = i.rs2.to_u5() } }).to_varinstr().to_memory(memory),
            .C_AND => |i| (IF16{ .ca = .{ .funct3 = 0b100, .op = 0b01, .offset = 0b011, .prd = i.rd.to_u3(), .funct2 = 0b11, .prs2 = i.rs2.to_u3() } }).to_varinstr().to_memory(memory),
            .C_OR => |i| (IF16{ .ca = .{ .funct3 = 0b100, .op = 0b01, .offset = 0b011, .prd = i.rd.to_u3(), .funct2 = 0b10, .prs2 = i.rs2.to_u3() } }).to_varinstr().to_memory(memory),
            .C_XOR => |i| (IF16{ .ca = .{ .funct3 = 0b100, .op = 0b01, .offset = 0b011, .prd = i.rd.to_u3(), .funct2 = 0b01, .prs2 = i.rs2.to_u3() } }).to_varinstr().to_memory(memory),
            .C_SUB => |i| (IF16{ .ca = .{ .funct3 = 0b100, .op = 0b01, .offset = 0b011, .prd = i.rd.to_u3(), .funct2 = 0b00, .prs2 = i.rs2.to_u3() } }).to_varinstr().to_memory(memory),
            .C_EBREAK => (IF16{ .ci = .{ .funct3 = 0b100, .op = 0b10, .imm_12 = 1, .rd = 0, .imm_2_6 = 0 } }).to_varinstr().to_memory(memory),
        };
    }

    pub fn from_memory(instr: base.VarInstr) ?Self {
        return switch (instr) {
            .x16 => |x16| {
                const i = base.InstrFormatX16.from_u16(x16);

                return switch (i.opcode) {
                    0b00 => switch (i.cl.funct3) {
                        0b000 => .{ .C_ADDI4SPN = .{ .uimm = i.ciw.imm, .rd = PIRf(i.ciw.prd) } },
                        0b010 => .{ .C_LW = .{ .uimm = (s(u5, i.cl.imm2) << 2) | s(u5, i.cl.imm1), .rs1 = PIRf(i.cl.prs1), .rd = PIRf(i.cl.prd) } },
                        0b011 => .{ .C_FLW = .{ .uimm = (s(u5, i.cl.imm2) << 2) | s(u5, i.cl.imm1), .rs1 = PIRf(i.cl.prs1), .rd = PIRf(i.cl.prd) } },
                        0b110 => Self{ .C_SW = .{ .uimm = (s(u5, i.cs.imm2) << 2) | s(u5, i.cs.imm1), .rs2 = PIRf(i.cs.prs2), .rs1 = PIRf(i.cs.prs1) } },
                        0b111 => Self{ .C_FSW = .{ .uimm = (s(u5, i.cs.imm2) << 2) | s(u5, i.cs.imm1), .rs2 = PIRf(i.cs.prs2), .rs1 = PIRf(i.cs.prs1) } },
                        else => null,
                    },
                    0b01 => switch (i.ci.funct3) {
                        0b001 => .{ .C_JAL = .{ .imm = i.cj.target } },
                        0b000 => .{ .C_ADDI = .{ .imm = cast(i6, (s(u6, i.ci.imm_12) << 5) | s(u6, i.ci.imm_2_6)), .rd = IRf(i.ci.rd) } },
                        0b010 => .{ .C_LI = .{ .imm = cast(i6, (s(u6, i.ci.imm_12) << 5) | s(u6, i.ci.imm_2_6)), .rd = IRf(i.ci.rd) } },
                        0b011 => if (i.ci.rd == 2)
                            Self{ .C_ADDI16SP = .{ .imm = cast(i6, (s(u6, i.ci.imm_12) << 5) | s(u6, i.ci.imm_2_6)) } }
                        else
                            .{ .C_LUI = .{ .imm = cast(i6, (s(u6, i.ci.imm_12) << 5) | s(u6, i.ci.imm_2_6)), .rd = IRf(i.ci.rd) } },
                        0b100 => switch (t(u2, i.cb.offset2)) {
                            0b00 => Self{ .C_SRLI = .{ .shamt = (s(u6, i.ci.imm_12) << 5) | s(u6, i.cb.offset1), .rd = PIRf(i.cb.prd) } },
                            0b01 => Self{ .C_SRAI = .{ .shamt = (s(u6, i.ci.imm_12) << 5) | s(u6, i.cb.offset1), .rd = PIRf(i.cb.prd) } },
                            0b10 => Self{ .C_ANDI = .{ .imm = cast(i6, (s(u6, i.ci.imm_12) << 5) | s(u6, i.cb.offset1)), .rd = PIRf(i.cb.prd) } },
                            0b11 => if (i.ci.imm_12 == 0)
                                switch (i.ca.funct2) {
                                    0b00 => Self{ .C_SUB = .{ .rd = PIRf(i.ca.prd), .rs2 = PIRf(i.ca.prs2) } },
                                    0b01 => Self{ .C_XOR = .{ .rd = PIRf(i.ca.prd), .rs2 = PIRf(i.ca.prs2) } },
                                    0b10 => Self{ .C_OR = .{ .rd = PIRf(i.ca.prd), .rs2 = PIRf(i.ca.prs2) } },
                                    0b11 => Self{ .C_AND = .{ .rd = PIRf(i.ca.prd), .rs2 = PIRf(i.ca.prs2) } },
                                }
                            else
                                null,
                        },
                        0b101 => .{ .C_J = .{ .imm = i.cj.target } },
                        0b110 => .{ .C_BEQZ = .{ .imm = cast(i8, (s(u8, i.cb.offset2) << 5) | s(u8, i.cb.offset1)), .rs1 = PIRf(i.cb.prd) } },
                        0b111 => .{ .C_BNEZ = .{ .imm = cast(i8, (s(u8, i.cb.offset2) << 5) | s(u8, i.cb.offset1)), .rs1 = PIRf(i.cb.prd) } },
                    },
                    0b10 => switch (i.ci.funct3) {
                        0b000 => .{ .C_SLLI = .{ .shamt = (s(u6, i.ci.imm_12) << 5) | s(u6, i.ci.imm_2_6), .rd = IRf(i.ci.rd) } },
                        0b010 => .{ .C_LWSP = .{ .uimm = (s(u6, i.ci.imm_12) << 5) | s(u6, i.ci.imm_2_6), .rd = IRf(i.ci.rd) } },
                        0b011 => .{ .C_FLWSP = .{ .uimm = (s(u6, i.ci.imm_12) << 5) | s(u6, i.ci.imm_2_6), .rd = IRf(i.ci.rd) } },
                        0b100 => switch (i.ci.imm_12) {
                            0 => if (i.cr.rd == 0)
                                Self{ .C_JR = .{ .rs1 = IRf(i.cr.rs2) } }
                            else
                                Self{ .C_MV = .{ .rd = IRf(i.cr.rd), .rs2 = IRf(i.cr.rs2) } },
                            1 => if (i.cr.rd == 0)
                                Self{ .C_JALR = .{ .rs1 = IRf(i.cr.rs2) } }
                            else if (i.cr.rd == 0 and i.cr.rs2 == 0)
                                Self.C_EBREAK
                            else
                                Self{ .C_ADD = .{ .rd = IRf(i.cr.rd), .rs2 = IRf(i.cr.rs2) } },
                        },
                        0b110 => .{ .C_SWSP = .{ .uimm = i.css.imm, .rs2 = IRf(i.css.rs2) } },
                        0b111 => .{ .C_FSWSP = .{ .uimm = i.css.imm, .rs2 = IRf(i.css.rs2) } },
                        else => null,
                    },
                    else => null,
                };
            },
            else => null,
        };
    }

    pub fn len(self: Self) usize {
        _ = self;
        return 2;
    }

    pub fn write(self: Self, writer: std.io.AnyWriter) !void {
        return switch (self) {
            .C_LWSP => |i| writer.print("C_LWSP {s}, 0x{x}\n", .{ i.rd.name(), i.uimm }),
            .C_FLWSP => |i| writer.print("C_FLWSP {s}, 0x{x}\n", .{ i.rd.name(), i.uimm }),
            .C_SWSP => |i| writer.print("C_SWSP {s}, 0x{x}\n", .{ i.rs2.name(), i.uimm }),
            .C_FSWSP => |i| writer.print("C_FSWSP {s}, 0x{x}\n", .{ i.rs2.name(), i.uimm }),
            .C_LW => |i| writer.print("C_LW {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.uimm }),
            .C_FLW => |i| writer.print("C_FLW {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.uimm }),
            .C_SW => |i| writer.print("C_SW {s}, {s}, 0x{x}\n", .{ i.rs1.name(), i.rs2.name(), i.uimm }),
            .C_FSW => |i| writer.print("C_FSW {s}, {s}, 0x{x}\n", .{ i.rs1.name(), i.rs2.name(), i.uimm }),
            .C_J => |i| writer.print("C_J {x}\n", .{i.imm}),
            .C_JAL => |i| writer.print("C_JAL {x}\n", .{i.imm}),
            .C_JR => |i| writer.print("C_JR {s}\n", .{i.rs1.name()}),
            .C_JALR => |i| writer.print("C_JALR {s}\n", .{i.rs1.name()}),
            .C_BEQZ => |i| writer.print("C_BEQZ {s}, {x}\n", .{ i.rs1.name(), i.imm }),
            .C_BNEZ => |i| writer.print("C_BNEZ {s}, {x}\n", .{ i.rs1.name(), i.imm }),
            .C_LI => |i| writer.print("C_LI {s}, {x}\n", .{ i.rd.name(), i.imm }),
            .C_LUI => |i| writer.print("C_LUI {s}, {x}\n", .{ i.rd.name(), i.imm }),
            .C_ADDI => |i| writer.print("C_ADDI {s}, {x}\n", .{ i.rd.name(), i.imm }),
            .C_ADDI16SP => |i| writer.print("C_ADDI16SP {x}\n", .{i.imm}),
            .C_ADDI4SPN => |i| writer.print("C_ADDI4SPN {s}, {x}\n", .{ i.rd.name(), i.uimm }),
            .C_SLLI => |i| writer.print("C_SLLI {s}, {x}\n", .{ i.rd.name(), i.shamt }),
            .C_SRLI => |i| writer.print("C_SRLI {s}, {x}\n", .{ i.rd.name(), i.shamt }),
            .C_SRAI => |i| writer.print("C_SRAI {s}, {x}\n", .{ i.rd.name(), i.shamt }),
            .C_ANDI => |i| writer.print("C_ANDI {s}, {x}\n", .{ i.rd.name(), i.imm }),
            .C_MV => |i| writer.print("C_MV {s}, {s}\n", .{ i.rd.name(), i.rs2.name() }),
            .C_ADD => |i| writer.print("C_ADD {s}, {s}\n", .{ i.rd.name(), i.rs2.name() }),
            .C_AND => |i| writer.print("C_AND {s}, {s}\n", .{ i.rd.name(), i.rs2.name() }),
            .C_OR => |i| writer.print("C_OR {s}, {s}\n", .{ i.rd.name(), i.rs2.name() }),
            .C_XOR => |i| writer.print("C_XOR {s}, {s}\n", .{ i.rd.name(), i.rs2.name() }),
            .C_SUB => |i| writer.print("C_SUB {s}, {s}\n", .{ i.rd.name(), i.rs2.name() }),
            .C_EBREAK => writer.print("C_EBREAK\n", .{}),
        };
    }

    pub fn used_grs(self: Self) [3]IR {
        return switch (self) {
            .C_LWSP => |i| .{ i.rd, IR.SP, IR.ZERO },
            .C_FLWSP => |i| .{ i.rd, IR.SP, IR.ZERO },
            .C_SWSP => |i| .{ i.rs2, IR.SP, IR.ZERO },
            .C_FSWSP => |i| .{ i.rs2, IR.SP, IR.ZERO },
            .C_LW => |i| .{ i.rd.to_reg(), i.rs1.to_reg(), IR.ZERO },
            .C_FLW => |i| .{ i.rd.to_reg(), i.rs1.to_reg(), IR.ZERO },
            .C_SW => |i| .{ i.rs1.to_reg(), i.rs2.to_reg(), IR.ZERO },
            .C_FSW => |i| .{ i.rs1.to_reg(), i.rs2.to_reg(), IR.ZERO },
            .C_J => |_| .{ IR.ZERO, IR.ZERO, IR.ZERO },
            .C_JAL => |_| .{ IR.ZERO, IR.ZERO, IR.ZERO },
            .C_JR => |i| .{ i.rs1, IR.ZERO, IR.ZERO },
            .C_JALR => |i| .{ i.rs1, IR.RA, IR.ZERO },
            .C_BEQZ => |i| .{ i.rs1.to_reg(), IR.ZERO, IR.ZERO },
            .C_BNEZ => |i| .{ i.rs1.to_reg(), IR.ZERO, IR.ZERO },
            .C_LI => |i| .{ i.rd, IR.ZERO, IR.ZERO },
            .C_LUI => |i| .{ i.rd, IR.ZERO, IR.ZERO },
            .C_ADDI => |i| .{ i.rd, IR.ZERO, IR.ZERO },
            .C_ADDI16SP => |_| .{ IR.SP, IR.ZERO, IR.ZERO },
            .C_ADDI4SPN => |_| .{ IR.SP, IR.ZERO, IR.ZERO },
            .C_SLLI => |i| .{ i.rd, IR.ZERO, IR.ZERO },
            .C_SRLI => |i| .{ i.rd.to_reg(), IR.ZERO, IR.ZERO },
            .C_SRAI => |i| .{ i.rd.to_reg(), IR.ZERO, IR.ZERO },
            .C_ANDI => |i| .{ i.rd.to_reg(), IR.ZERO, IR.ZERO },
            .C_MV => |i| .{ i.rd, i.rs2, IR.ZERO },
            .C_ADD => |i| .{ i.rd, i.rs2, IR.ZERO },
            .C_AND => |i| .{ i.rd.to_reg(), i.rs2.to_reg(), IR.ZERO },
            .C_OR => |i| .{ i.rd.to_reg(), i.rs2.to_reg(), IR.ZERO },
            .C_XOR => |i| .{ i.rd.to_reg(), i.rs2.to_reg(), IR.ZERO },
            .C_SUB => |i| .{ i.rd.to_reg(), i.rs2.to_reg(), IR.ZERO },
            .C_EBREAK => .{ IR.ZERO, IR.ZERO, IR.ZERO },
        };
    }
};

pub const RV64C = union(enum) {
    C_LDSP: struct { rd: IR, uimm: u6 },
    C_FLDSP: struct { rd: IR, uimm: u6 },
    C_SDSP: struct { rd: IR, uimm: u6 },
    C_FSDSP: struct { rd: IR, uimm: u6 },
    C_LD: struct { rd: PIR, rs1: PIR, uimm: u5 },
    C_FLD: struct { rd: PIR, rs1: PIR, uimm: u5 },
    C_ADDIW: struct { rd: IR, imm: i6 },
    C_ADDW: struct { rd: PIR, rs2: PIR },
    C_SUBW: struct { rd: PIR, rs2: PIR },

    const Self = @This();

    pub fn to_memory(self: Self, memory: []u8) error{OutOfSpace}!usize {
        return switch (self) {
            .C_LDSP => |i| (base.InstrFormatX16{ .ci = .{ .funct3 = 0b011, .imm_12 = t(u1, i.uimm >> 5), .rd = i.rd.to_u5(), .imm_2_6 = t(u5, i.uimm), .op = 0b10 } }).to_varinstr().to_memory(memory),
            .C_FLDSP => |i| (base.InstrFormatX16{ .ci = .{ .funct3 = 0b001, .imm_12 = t(u1, i.uimm >> 5), .rd = i.rd.to_u5(), .imm_2_6 = t(u5, i.uimm), .op = 0b10 } }).to_varinstr().to_memory(memory),
            .C_SDSP => |i| (base.InstrFormatX16{ .css = .{ .funct3 = 0b111, .imm = i.uimm, .rs2 = i.rd.to_u5(), .op = 0b10 } }).to_varinstr().to_memory(memory),
            .C_FSDSP => |i| (base.InstrFormatX16{ .css = .{ .funct3 = 0b101, .imm = i.uimm, .rs2 = i.rd.to_u5(), .op = 0b10 } }).to_varinstr().to_memory(memory),
            .C_LD => |i| (base.InstrFormatX16{ .cl = .{ .funct3 = 0b011, .imm2 = t(u3, i.uimm), .prs1 = i.rs1.to_u3(), .imm1 = t(u2, i.uimm << 3), .prd = i.rd.to_u3(), .op = 0b00 } }).to_varinstr().to_memory(memory),
            .C_FLD => |i| (base.InstrFormatX16{ .cl = .{ .funct3 = 0b001, .imm2 = t(u3, i.uimm), .prs1 = i.rs1.to_u3(), .imm1 = t(u2, i.uimm << 3), .prd = i.rd.to_u3(), .op = 0b00 } }).to_varinstr().to_memory(memory),
            .C_ADDIW => |i| (base.InstrFormatX16{ .ci = .{ .funct3 = 0b001, .imm_12 = t(u1, cast(u6, i.imm) >> 5), .rd = i.rd.to_u5(), .imm_2_6 = t(u5, cast(u6, i.imm)), .op = 0b01 } }).to_varinstr().to_memory(memory),
            .C_ADDW => |i| (base.InstrFormatX16{ .ca = .{ .funct3 = 0b100, .offset = 0b111, .prd = i.rd.to_u3(), .funct2 = 0b01, .prs2 = i.rs2.to_u3(), .op = 0b01 } }).to_varinstr().to_memory(memory),
            .C_SUBW => |i| (base.InstrFormatX16{ .ca = .{ .funct3 = 0b100, .offset = 0b111, .prd = i.rd.to_u3(), .funct2 = 0b00, .prs2 = i.rs2.to_u3(), .op = 0b01 } }).to_varinstr().to_memory(memory),
        };
    }

    pub fn from_memory(instr: base.VarInstr) ?Self {
        return switch (instr) {
            .x16 => |x16| {
                const i = base.InstrFormatX16.from_u16(x16);

                return switch (i.opcode) {
                    0b00 => switch (i.cl.funct3) {
                        0b001 => .{ .C_LD = .{ .uimm = (s(u5, i.cl.imm1) << 3) + s(u5, i.cl.imm2), .rs1 = PIRf(i.cl.prs1), .rd = PIRf(i.cl.prd) } },
                        0b011 => .{ .C_FLD = .{ .uimm = (s(u5, i.cl.imm1) << 3) + s(u5, i.cl.imm2), .rs1 = PIRf(i.cl.prs1), .rd = PIRf(i.cl.prd) } },
                        else => null,
                    },
                    0b01 => switch (i.ca.funct3) {
                        0b001 => Self{ .C_ADDIW = .{ .imm = cast(i6, (s(u6, i.ci.imm_12) << 5) + s(u6, i.ci.imm_2_6)), .rd = IRf(i.ci.rd) } },
                        0b100 => switch (i.ci.imm_12) {
                            0 => switch (t(u2, i.cb.funct3 >> 2)) {
                                else => null,
                            },
                            1 => switch (i.ca.funct2) {
                                0b00 => .{ .C_SUBW = .{ .rd = PIRf(i.ca.prd), .rs2 = PIRf(i.ca.prs2) } },
                                0b01 => .{ .C_ADDW = .{ .rd = PIRf(i.ca.prd), .rs2 = PIRf(i.ca.prs2) } },
                                else => null,
                            },
                        },
                        else => null,
                    },
                    0b10 => switch (i.ci.funct3) {
                        0b001 => .{ .C_FLDSP = .{ .rd = IRf(i.ci.rd), .uimm = (s(u6, i.ci.imm_12) << 5) + s(u6, i.ci.imm_2_6) } },
                        0b011 => .{ .C_LDSP = .{ .rd = IRf(i.ci.rd), .uimm = (s(u6, i.ci.imm_12) << 5) + s(u6, i.ci.imm_2_6) } },
                        0b101 => .{ .C_FSDSP = .{ .rd = IRf(i.css.rs2), .uimm = i.css.imm } },
                        0b111 => .{ .C_SDSP = .{ .rd = IRf(i.css.rs2), .uimm = i.css.imm } },
                        else => null,
                    },
                    else => null,
                };
            },
            else => null,
        };
    }

    pub fn len(self: Self) usize {
        _ = self;
        return 2;
    }

    pub fn write(self: Self, writer: std.io.AnyWriter) !void {
        return switch (self) {
            .C_LDSP => |i| writer.print("C_LDSP {s}, 0x{x}\n", .{ i.rd.name(), i.uimm }),
            .C_FLDSP => |i| writer.print("C_FLDSP {s}, 0x{x}\n", .{ i.rd.name(), i.uimm }),
            .C_SDSP => |i| writer.print("C_SDSP {s}, 0x{x}\n", .{ i.rd.name(), i.uimm }),
            .C_FSDSP => |i| writer.print("C_FSDSP {s}, 0x{x}\n", .{ i.rd.name(), i.uimm }),
            .C_LD => |i| writer.print("C_LD {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.uimm }),
            .C_FLD => |i| writer.print("C_FLD {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.uimm }),
            .C_ADDIW => |i| writer.print("C_ADDIW {s}, {x}\n", .{ i.rd.name(), i.imm }),
            .C_ADDW => |i| writer.print("C_ADDW {s}, {s}\n", .{ i.rd.name(), i.rs2.name() }),
            .C_SUBW => |i| writer.print("C_SUBW {s}, {s}\n", .{ i.rd.name(), i.rs2.name() }),
        };
    }

    pub fn used_grs(self: Self) [3]IR {
        return switch (self) {
            .C_LDSP => |i| .{ i.rd, IR.ZERO, IR.ZERO },
            .C_FLDSP => |i| .{ i.rd, IR.ZERO, IR.ZERO },
            .C_SDSP => |i| .{ i.rd, IR.ZERO, IR.ZERO },
            .C_FSDSP => |i| .{ i.rd, IR.ZERO, IR.ZERO },
            .C_LD => |i| .{ i.rd.to_reg(), i.rs1.to_reg(), IR.ZERO },
            .C_FLD => |i| .{ i.rd.to_reg(), i.rs1.to_reg(), IR.ZERO },
            .C_ADDIW => |i| .{ i.rd, IR.ZERO, IR.ZERO },
            .C_ADDW => |i| .{ i.rd.to_reg(), i.rs2.to_reg(), IR.ZERO },
            .C_SUBW => |i| .{ i.rd.to_reg(), i.rs2.to_reg(), IR.ZERO },
        };
    }
};

pub fn build_asm(comptime arch: base.Arch) type {
    return switch (arch) {
        .X32 => union(enum) {
            rv32i: RV32I,
            z_iscr: Ziscr,
            rv32m: RV32M,
            rv32c: RV32C,

            const Self = @This();

            pub fn to_memory(self: Self, memory: []u8) !usize {
                return switch (self) {
                    .rv32i => |i| i.to_memory(memory),
                    .z_iscr => |i| i.to_memory(memory),
                    .rv32m => |i| i.to_memory(memory),
                    .rv32c => |i| i.to_memory(memory),
                };
            }

            pub fn from_memory(memory: []const u8) error{ EndOfStream, VarInstrNotImplemented, Unimplemented }!Self {
                const v = try base.VarInstr.from_memory(memory);
                if (RV32I.from_memory(v)) |i| {
                    return .{ .rv32i = i };
                }
                if (Ziscr.from_memory(v)) |i| {
                    return .{ .z_iscr = i };
                }
                if (RV32M.from_memory(v)) |i| {
                    return .{ .rv32m = i };
                }
                if (RV32C.from_memory(v)) |i| {
                    return .{ .rv32c = i };
                }

                v.debug();
                return error.Unimplemented;
            }

            pub fn len(self: Self) usize {
                return switch (self) {
                    .rv32i => |i| i.len(),
                    .z_iscr => |i| i.len(),
                    .rv32m => |i| i.len(),
                    .rv32c => |i| i.len(),
                };
            }

            pub fn write(self: Self, writer: std.io.AnyWriter) !void {
                return switch (self) {
                    .rv32i => |i| i.write(writer),
                    .z_iscr => |i| i.write(writer),
                    .rv32m => |i| i.write(writer),
                    .rv32c => |i| i.write(writer),
                };
            }

            pub fn used_grs(self: Self) [3]IR {
                return switch (self) {
                    .rv32i => |i| i.used_grs(),
                    .z_iscr => |i| i.used_grs(),
                    .rv32m => |i| i.used_grs(),
                    .rv32c => |i| i.used_grs(),
                };
            }
        },
        .X64 => union(enum) {
            rv32i: RV32I,
            rv64i: RV64I,
            z_iscr: Ziscr,
            rv32m: RV32M,
            rv32c: RV32C,
            rv64m: RV64M,
            rv64c: RV64C,

            const Self = @This();

            pub fn to_memory(self: Self, memory: []u8) !usize {
                return switch (self) {
                    .rv32i => |i| i.to_memory(memory),
                    .rv64i => |i| i.to_memory(memory),
                    .z_iscr => |i| i.to_memory(memory),
                    .rv32m => |i| i.to_memory(memory),
                    .rv64m => |i| i.to_memory(memory),
                    .rv32c => |i| i.to_memory(memory),
                    .rv64c => |i| i.to_memory(memory),
                };
            }

            pub fn from_memory(memory: []const u8) error{ EndOfStream, VarInstrNotImplemented, Unimplemented }!Self {
                const v = try base.VarInstr.from_memory(memory);
                if (RV64I.from_memory(v)) |i| {
                    return .{ .rv64i = i };
                }
                if (RV32I.from_memory(v)) |i| {
                    return .{ .rv32i = i };
                }
                if (Ziscr.from_memory(v)) |i| {
                    return .{ .z_iscr = i };
                }
                if (RV64M.from_memory(v)) |i| {
                    return .{ .rv64m = i };
                }
                if (RV32M.from_memory(v)) |i| {
                    return .{ .rv32m = i };
                }
                if (RV64C.from_memory(v)) |i| {
                    return .{ .rv64c = i };
                }
                if (RV32C.from_memory(v)) |i| {
                    return .{ .rv32c = i };
                }

                v.debug();
                return error.Unimplemented;
            }

            pub fn len(self: Self) usize {
                return switch (self) {
                    .rv32i => |i| i.len(),
                    .rv64i => |i| i.len(),
                    .z_iscr => |i| i.len(),
                    .rv32m => |i| i.len(),
                    .rv64m => |i| i.len(),
                    .rv32c => |i| i.len(),
                    .rv64c => |i| i.len(),
                };
            }

            pub fn write(self: Self, writer: std.io.AnyWriter) !void {
                return switch (self) {
                    .rv32i => |i| i.write(writer),
                    .rv64i => |i| i.write(writer),
                    .z_iscr => |i| i.write(writer),
                    .rv32m => |i| i.write(writer),
                    .rv64m => |i| i.write(writer),
                    .rv32c => |i| i.write(writer),
                    .rv64c => |i| i.write(writer),
                };
            }

            pub fn used_grs(self: Self) [3]IR {
                return switch (self) {
                    .rv32i => |i| i.used_grs(),
                    .rv64i => |i| i.used_grs(),
                    .z_iscr => |i| i.used_grs(),
                    .rv32m => |i| i.used_grs(),
                    .rv64m => |i| i.used_grs(),
                    .rv32c => |i| i.used_grs(),
                    .rv64c => |i| i.used_grs(),
                };
            }
        },
    };
}
