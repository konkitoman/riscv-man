const std = @import("std");
const log = std.log;
const base = @import("base.zig");

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

pub fn build_asm(comptime arch: base.Arch) type {
    return switch (arch) {
        .X32 => union(enum) {
            rv32i: RV32I,
            z_iscr: Ziscr,

            const Self = @This();

            pub fn to_memory(self: Self, memory: []u8) !usize {
                return switch (self) {
                    .rv32i => |i| i.to_memory(memory),
                    .z_iscr => |i| i.to_memory(memory),
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

                v.debug();
                return error.Unimplemented;
            }

            pub fn len(self: Self) usize {
                return switch (self) {
                    .rv32i => |i| i.len(),
                    .z_iscr => |i| i.len(),
                };
            }

            pub fn write(self: Self, writer: std.io.AnyWriter) !void {
                return switch (self) {
                    .rv32i => |i| i.write(writer),
                    .z_iscr => |i| i.write(writer),
                };
            }

            pub fn used_grs(self: Self) [3]IR {
                return switch (self) {
                    .rv32i => |i| i.used_grs(),
                    .z_iscr => |i| i.used_grs(),
                };
            }
        },
        .X64 => union(enum) {
            rv32i: RV32I,
            rv64i: RV64I,
            z_iscr: Ziscr,

            const Self = @This();

            pub fn to_memory(self: Self, memory: []u8) !usize {
                return switch (self) {
                    .rv32i => |i| i.to_memory(memory),
                    .rv64i => |i| i.to_memory(memory),
                    .z_iscr => |i| i.to_memory(memory),
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

                v.debug();
                return error.Unimplemented;
            }

            pub fn len(self: Self) usize {
                return switch (self) {
                    .rv32i => |i| i.len(),
                    .rv64i => |i| i.len(),
                    .z_iscr => |i| i.len(),
                };
            }

            pub fn write(self: Self, writer: std.io.AnyWriter) !void {
                return switch (self) {
                    .rv32i => |i| i.write(writer),
                    .rv64i => |i| i.write(writer),
                    .z_iscr => |i| i.write(writer),
                };
            }

            pub fn used_grs(self: Self) [3]IR {
                return switch (self) {
                    .rv32i => |i| i.used_grs(),
                    .rv64i => |i| i.used_grs(),
                    .z_iscr => |i| i.used_grs(),
                };
            }
        },
        .X128 => union(enum) {
            rv32i: RV32I,
            rv64i: RV64I,
            z_iscr: Ziscr,
        },
    };
}
