const std = @import("std");
const log = std.log;
const base = @import("base.zig");

const GR = base.GeneralReg;
const GRf = GR.from_u5;

// # is for when are declared
// a is for assembly helpers
// w is for writing to memory
// f is for format

inline fn cast(T: type, value: anytype) T {
    return @bitCast(value);
}

pub const Instr = union(enum) {
    // # RV32I

    LUI: struct { rd: GR, imm: u20 },
    AUIPC: struct { rd: GR, imm: u20 },
    JAL: struct { rd: GR, imm: i20 },
    JALR: struct { rd: GR, rs1: GR, imm: i12 },
    BEQ: struct { rs1: GR, rs2: GR, offset: i12 },
    BNE: struct { rs1: GR, rs2: GR, offset: i12 },
    BLT: struct { rs1: GR, rs2: GR, offset: i12 },
    BGE: struct { rs1: GR, rs2: GR, offset: i12 },
    BLTU: struct { rs1: GR, rs2: GR, offset: i12 },
    BGEU: struct { rs1: GR, rs2: GR, offset: i12 },
    LB: struct { rd: GR, rs1: GR, offset: i12 },
    LH: struct { rd: GR, rs1: GR, offset: i12 },
    LW: struct { rd: GR, rs1: GR, offset: i12 },
    LBU: struct { rd: GR, rs1: GR, offset: i12 },
    LHU: struct { rd: GR, rs1: GR, offset: i12 },
    SB: struct { rs1: GR, rs2: GR, offset: i12 },
    SH: struct { rs1: GR, rs2: GR, offset: i12 },
    SW: struct { rs1: GR, rs2: GR, offset: i12 },
    ADDI: struct { rd: GR, rs1: GR, imm: i12 },
    SLTI: struct { rd: GR, rs1: GR, imm: i12 },
    SLTIU: struct { rd: GR, rs1: GR, imm: i12 },
    XORI: struct { rd: GR, rs1: GR, imm: i12 },
    ORI: struct { rd: GR, rs1: GR, imm: i12 },
    ANDI: struct { rd: GR, rs1: GR, imm: i12 },
    SLLI: struct { rd: GR, rs1: GR, shamt: u6 },
    SRLI: struct { rd: GR, rs1: GR, shamt: u6 },
    SRAI: struct { rd: GR, rs1: GR, shamt: u6 },
    ADD: struct { rd: GR, rs1: GR, rs2: GR },
    SUB: struct { rd: GR, rs1: GR, rs2: GR },
    SLL: struct { rd: GR, rs1: GR, rs2: GR },
    SLT: struct { rd: GR, rs1: GR, rs2: GR },
    SLTU: struct { rd: GR, rs1: GR, rs2: GR },
    XOR: struct { rd: GR, rs1: GR, rs2: GR },
    SRL: struct { rd: GR, rs1: GR, rs2: GR },
    SRA: struct { rd: GR, rs1: GR, rs2: GR },
    OR: struct { rd: GR, rs1: GR, rs2: GR },
    AND: struct { rd: GR, rs1: GR, rs2: GR },
    FENCE: struct { rd: GR, rs1: GR, succ: base.FFlags, pred: base.FFlags, fm: u4 },
    ECALL,
    EBREAK,

    // # RV64I

    LWU: struct { rd: GR, rs1: GR, offset: i12 },
    LD: struct { rd: GR, rs1: GR, offset: i12 },
    SD: struct { rs1: GR, rs2: GR, offset: i12 },
    // SLLI from RV32I
    // SRLI from RV32I
    // SRAI from RV32I
    ADDIW: struct { rd: GR, rs1: GR, imm: i12 },
    SLLIW: struct { rd: GR, rs1: GR, shamt: u6 },
    SRLIW: struct { rd: GR, rs1: GR, shamt: u6 },
    SRAIW: struct { rd: GR, rs1: GR, shamt: u6 },
    ADDW: struct { rd: GR, rs1: GR, rs2: GR },
    SUBW: struct { rd: GR, rs1: GR, rs2: GR },
    SLLW: struct { rd: GR, rs1: GR, rs2: GR },
    SRLW: struct { rd: GR, rs1: GR, rs2: GR },
    SRAW: struct { rd: GR, rs1: GR, rs2: GR },

    // # Ziscr

    CSRRW: struct { rd: GR, rs1: GR, csr: u12 },
    CSRRS: struct { rd: GR, rs1: GR, csr: u12 },
    CSRRC: struct { rd: GR, rs1: GR, csr: u12 },
    CSRRWI: struct { rd: GR, uimm: u5, csr: u12 },
    CSRRSI: struct { rd: GR, uimm: u5, csr: u12 },
    CSRRCI: struct { rd: GR, uimm: u5, csr: u12 },

    UNKNOWN_X16: u16,
    UNKNOWN_X32: u32,
    UNKNOWN_X64: u64,

    const Self = @This();

    pub fn to_memory(self: Self, memory: []u8) error{OutOfSpace}!usize {
        const vari = switch (self) {
            // w RV32I
            .LUI => |i| (base.ImmediateVariantX32{ .u = .{ .imm_31_12 = i.imm, .rd = i.rd.to_u5(), .opcode = 0b0110111 } }).to_varinstr(),
            .AUIPC => |i| (base.ImmediateVariantX32{ .u = .{ .imm_31_12 = i.imm, .rd = i.rd.to_u5(), .opcode = 0b0010111 } }).to_varinstr(),
            .JAL => |i| (base.ImmediateVariantX32{ .j = .{ .imm_10_1 = @truncate(cast(u20, i.imm)), .b_11 = @truncate(cast(u20, i.imm) >> 10), .imm_19_12 = @truncate((cast(u20, i.imm) >> 11)), .b_20 = @truncate(cast(u20, i.imm) >> 19), .rd = i.rd.to_u5(), .opcode = 0b1101111 } }).to_varinstr(),
            .JALR => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = i.imm, .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b1100111 } }).to_varinstr(),
            .BEQ => |i| (base.ImmediateVariantX32{ .b = .{ .imm_12 = @truncate((cast(u12, i.offset) >> 11) & 1), .imm_11 = @truncate(cast(u12, i.offset) >> 10), .imm_10_5 = @truncate(cast(u12, i.offset) >> 4), .imm_4_1 = @truncate(cast(u12, i.offset)), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .opcode = 0b1100011 } }).to_varinstr(),
            .BNE => |i| (base.ImmediateVariantX32{ .b = .{ .imm_12 = @truncate((cast(u12, i.offset) >> 11) & 1), .imm_11 = @truncate(cast(u12, i.offset) >> 10), .imm_10_5 = @truncate(cast(u12, i.offset) >> 4), .imm_4_1 = @truncate(cast(u12, i.offset)), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .opcode = 0b1100011 } }).to_varinstr(),
            .BLT => |i| (base.ImmediateVariantX32{ .b = .{ .imm_12 = @truncate((cast(u12, i.offset) >> 11) & 1), .imm_11 = @truncate(cast(u12, i.offset) >> 10), .imm_10_5 = @truncate(cast(u12, i.offset) >> 4), .imm_4_1 = @truncate(cast(u12, i.offset)), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b100, .opcode = 0b1100011 } }).to_varinstr(),
            .BGE => |i| (base.ImmediateVariantX32{ .b = .{ .imm_12 = @truncate((cast(u12, i.offset) >> 11) & 1), .imm_11 = @truncate(cast(u12, i.offset) >> 10), .imm_10_5 = @truncate(cast(u12, i.offset) >> 4), .imm_4_1 = @truncate(cast(u12, i.offset)), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .opcode = 0b1100011 } }).to_varinstr(),
            .BLTU => |i| (base.ImmediateVariantX32{ .b = .{ .imm_12 = @truncate((cast(u12, i.offset) >> 11) & 1), .imm_11 = @truncate(cast(u12, i.offset) >> 10), .imm_10_5 = @truncate(cast(u12, i.offset) >> 4), .imm_4_1 = @truncate(cast(u12, i.offset)), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b110, .opcode = 0b1100011 } }).to_varinstr(),
            .BGEU => |i| (base.ImmediateVariantX32{ .b = .{ .imm_12 = @truncate((cast(u12, i.offset) >> 11) & 1), .imm_11 = @truncate(cast(u12, i.offset) >> 10), .imm_10_5 = @truncate(cast(u12, i.offset) >> 4), .imm_4_1 = @truncate(cast(u12, i.offset)), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b111, .opcode = 0b1100011 } }).to_varinstr(),
            .LB => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = i.offset, .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b0000011 } }).to_varinstr(),
            .LH => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = i.offset, .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .rd = i.rd.to_u5(), .opcode = 0b0000011 } }).to_varinstr(),
            .LW => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = i.offset, .rs1 = i.rs1.to_u5(), .funct3 = 0b010, .rd = i.rd.to_u5(), .opcode = 0b0000011 } }).to_varinstr(),
            .LBU => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = i.offset, .rs1 = i.rs1.to_u5(), .funct3 = 0b100, .rd = i.rd.to_u5(), .opcode = 0b0000011 } }).to_varinstr(),
            .LHU => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = i.offset, .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0000011 } }).to_varinstr(),
            .SB => |i| (base.ImmediateVariantX32{ .s = .{ .imm_11_5 = @truncate(cast(u12, i.offset) >> 5), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .imm_4_0 = @truncate(cast(u12, i.offset)), .opcode = 0b0100011 } }).to_varinstr(),
            .SH => |i| (base.ImmediateVariantX32{ .s = .{ .imm_11_5 = @truncate(cast(u12, i.offset) >> 5), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .imm_4_0 = @truncate(cast(u12, i.offset)), .opcode = 0b0100011 } }).to_varinstr(),
            .SW => |i| (base.ImmediateVariantX32{ .s = .{ .imm_11_5 = @truncate(cast(u12, i.offset) >> 5), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b010, .imm_4_0 = @truncate(cast(u12, i.offset)), .opcode = 0b0100011 } }).to_varinstr(),
            .ADDI => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = i.imm, .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .SLTI => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = i.imm, .rs1 = i.rs1.to_u5(), .funct3 = 0b010, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .SLTIU => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = i.imm, .rs1 = i.rs1.to_u5(), .funct3 = 0b011, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .XORI => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = i.imm, .rs1 = i.rs1.to_u5(), .funct3 = 0b100, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .ORI => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = i.imm, .rs1 = i.rs1.to_u5(), .funct3 = 0b110, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .ANDI => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = i.imm, .rs1 = i.rs1.to_u5(), .funct3 = 0b111, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .SLLI => |i| (base.ImmediateVariantX32{ .i_1 = .{ .op = 0b000000, .shamt = i.shamt, .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .SRLI => |i| (base.ImmediateVariantX32{ .i_1 = .{ .op = 0b000000, .shamt = i.shamt, .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .SRAI => |i| (base.ImmediateVariantX32{ .i_1 = .{ .op = 0b010000, .shamt = i.shamt, .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0010011 } }).to_varinstr(),
            .ADD => |i| (base.ImmediateVariantX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .SUB => |i| (base.ImmediateVariantX32{ .r = .{ .funct7 = 0b0100000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .SLL => |i| (base.ImmediateVariantX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .SLT => |i| (base.ImmediateVariantX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b010, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .SLTU => |i| (base.ImmediateVariantX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b011, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .XOR => |i| (base.ImmediateVariantX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b100, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .SRL => |i| (base.ImmediateVariantX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .SRA => |i| (base.ImmediateVariantX32{ .r = .{ .funct7 = 0b0100000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .OR => |i| (base.ImmediateVariantX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b110, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .AND => |i| (base.ImmediateVariantX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b111, .rd = i.rd.to_u5(), .opcode = 0b0110011 } }).to_varinstr(),
            .FENCE => |i| (base.ImmediateVariantX32{ .f = .{ .fm = i.fm, .pred = i.pred, .succ = i.succ, .rs1 = i.rs1.to_u5(), .func3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b0001111 } }).to_varinstr(),
            .ECALL => (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = 0, .rs1 = 0, .funct3 = 0, .rd = 0, .opcode = 0b1110011 } }).to_varinstr(),
            .EBREAK => (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = 1, .rs1 = 0, .funct3 = 0, .rd = 0, .opcode = 0b1110011 } }).to_varinstr(),

            // w RV64I
            .LWU => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = i.offset, .rs1 = i.rs1.to_u5(), .funct3 = 0b110, .rd = i.rd.to_u5(), .opcode = 0b0000011 } }).to_varinstr(),
            .LD => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = i.offset, .rs1 = i.rs1.to_u5(), .funct3 = 0b011, .rd = i.rd.to_u5(), .opcode = 0b0000011 } }).to_varinstr(),
            .SD => |i| (base.ImmediateVariantX32{ .s = .{ .imm_11_5 = @truncate(cast(u12, i.offset) >> 5), .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b011, .imm_4_0 = @truncate(cast(u12, i.offset)), .opcode = 0b0100011 } }).to_varinstr(),
            .ADDIW => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = i.imm, .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b0011011 } }).to_varinstr(),
            .SLLIW => |i| (base.ImmediateVariantX32{ .i_1 = .{ .op = 0b000000, .shamt = i.shamt, .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .rd = i.rd.to_u5(), .opcode = 0b0011011 } }).to_varinstr(),
            .SRLIW => |i| (base.ImmediateVariantX32{ .i_1 = .{ .op = 0b000000, .shamt = i.shamt, .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0011011 } }).to_varinstr(),
            .SRAIW => |i| (base.ImmediateVariantX32{ .i_1 = .{ .op = 0b010000, .shamt = i.shamt, .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0011011 } }).to_varinstr(),
            .ADDW => |i| (base.ImmediateVariantX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b0111011 } }).to_varinstr(),
            .SUBW => |i| (base.ImmediateVariantX32{ .r = .{ .funct7 = 0b0100000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b000, .rd = i.rd.to_u5(), .opcode = 0b0111011 } }).to_varinstr(),
            .SLLW => |i| (base.ImmediateVariantX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .rd = i.rd.to_u5(), .opcode = 0b0111011 } }).to_varinstr(),
            .SRLW => |i| (base.ImmediateVariantX32{ .r = .{ .funct7 = 0b0000000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0111011 } }).to_varinstr(),
            .SRAW => |i| (base.ImmediateVariantX32{ .r = .{ .funct7 = 0b0100000, .rs2 = i.rs2.to_u5(), .rs1 = i.rs1.to_u5(), .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b0111011 } }).to_varinstr(),

            // w Ziscr
            .CSRRW => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = @bitCast(i.csr), .rs1 = i.rs1.to_u5(), .funct3 = 0b001, .rd = i.rd.to_u5(), .opcode = 0b1110011 } }).to_varinstr(),
            .CSRRS => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = @bitCast(i.csr), .rs1 = i.rs1.to_u5(), .funct3 = 0b010, .rd = i.rd.to_u5(), .opcode = 0b1110011 } }).to_varinstr(),
            .CSRRC => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = @bitCast(i.csr), .rs1 = i.rs1.to_u5(), .funct3 = 0b011, .rd = i.rd.to_u5(), .opcode = 0b1110011 } }).to_varinstr(),
            .CSRRWI => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = @bitCast(i.csr), .rs1 = i.uimm, .funct3 = 0b101, .rd = i.rd.to_u5(), .opcode = 0b1110011 } }).to_varinstr(),
            .CSRRSI => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = @bitCast(i.csr), .rs1 = i.uimm, .funct3 = 0b110, .rd = i.rd.to_u5(), .opcode = 0b1110011 } }).to_varinstr(),
            .CSRRCI => |i| (base.ImmediateVariantX32{ .i = .{ .imm_11_0 = @bitCast(i.csr), .rs1 = i.uimm, .funct3 = 0b111, .rd = i.rd.to_u5(), .opcode = 0b1110011 } }).to_varinstr(),

            .UNKNOWN_X16 => |data| base.VarInstr.X16(data),
            .UNKNOWN_X32 => |data| base.VarInstr.X32(data),
            .UNKNOWN_X64 => |data| base.VarInstr.X64(data),
        };

        return vari.to_memory(memory);
    }

    pub fn from_memory(memory: []const u8) error{ EndOfStream, VarInstrNotImplemented, Unimplemented }!Self {
        const v = try base.VarInstr.from_memory(memory);
        return switch (v) {
            .x16 => |x16| .{ .UNKNOWN_X16 = x16 },
            .x32 => |x32| {
                const i = base.ImmediateVariantX32.from_u32(x32);
                return switch (i.opcode) {
                    0b0110111 => .{ .LUI = .{ .rd = GRf(i.u.rd), .imm = i.u.imm_31_12 } },
                    0b0010111 => .{ .AUIPC = .{ .rd = GRf(i.u.rd), .imm = i.u.imm_31_12 } },
                    0b1101111 => .{ .JAL = .{ .rd = GRf(i.j.rd), .imm = i.j.get_imm() } },
                    0b1100111 => switch (i.i.funct3) {
                        0b000 => .{ .JALR = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .imm = i.i.imm_11_0 } },
                        else => return .{ .UNKNOWN_X32 = x32 },
                    },
                    0b1100011 => switch (i.b.funct3) {
                        0b000 => .{ .BEQ = .{ .rs1 = GRf(i.b.rs1), .rs2 = GRf(i.b.rs2), .offset = i.b.get_imm() } },
                        0b001 => .{ .BNE = .{ .rs1 = GRf(i.b.rs1), .rs2 = GRf(i.b.rs2), .offset = i.b.get_imm() } },
                        0b100 => .{ .BLT = .{ .rs1 = GRf(i.b.rs1), .rs2 = GRf(i.b.rs2), .offset = i.b.get_imm() } },
                        0b101 => .{ .BGE = .{ .rs1 = GRf(i.b.rs1), .rs2 = GRf(i.b.rs2), .offset = i.b.get_imm() } },
                        0b110 => .{ .BLTU = .{ .rs1 = GRf(i.b.rs1), .rs2 = GRf(i.b.rs2), .offset = i.b.get_imm() } },
                        0b111 => .{ .BGEU = .{ .rs1 = GRf(i.b.rs1), .rs2 = GRf(i.b.rs2), .offset = i.b.get_imm() } },
                        else => .{ .UNKNOWN_X32 = x32 },
                    },
                    0b0000011 => switch (i.i.funct3) {
                        0b000 => .{ .LB = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .offset = i.i.imm_11_0 } },
                        0b001 => .{ .LH = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .offset = i.i.imm_11_0 } },
                        0b010 => .{ .LW = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .offset = i.i.imm_11_0 } },
                        0b100 => .{ .LBU = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .offset = i.i.imm_11_0 } },
                        0b101 => .{ .LHU = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .offset = i.i.imm_11_0 } },

                        // RV64I
                        0b110 => .{ .LWU = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .offset = i.i.imm_11_0 } },
                        0b011 => .{ .LD = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .offset = i.i.imm_11_0 } },
                        else => return .{ .UNKNOWN_X32 = x32 },
                    },
                    0b0100011 => switch (i.s.funct3) {
                        0b000 => .{ .SB = .{ .rs1 = GRf(i.s.rs1), .rs2 = GRf(i.s.rs2), .offset = i.s.get_imm() } },
                        0b001 => .{ .SH = .{ .rs1 = GRf(i.s.rs1), .rs2 = GRf(i.s.rs2), .offset = i.s.get_imm() } },
                        0b010 => .{ .SW = .{ .rs1 = GRf(i.s.rs1), .rs2 = GRf(i.s.rs2), .offset = i.s.get_imm() } },

                        // RV64I
                        0b011 => .{ .SD = .{ .rs1 = GRf(i.s.rs1), .rs2 = GRf(i.s.rs2), .offset = i.s.get_imm() } },
                        else => .{ .UNKNOWN_X32 = x32 },
                    },
                    0b0010011 => switch (i.i.funct3) {
                        0b000 => .{ .ADDI = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .imm = i.i.imm_11_0 } },
                        0b010 => .{ .SLTI = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .imm = i.i.imm_11_0 } },
                        0b011 => .{ .SLTIU = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .imm = i.i.imm_11_0 } },
                        0b100 => .{ .XORI = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .imm = i.i.imm_11_0 } },
                        0b110 => .{ .ORI = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .imm = i.i.imm_11_0 } },
                        0b111 => .{ .ANDI = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .imm = i.i.imm_11_0 } },
                        0b001 => .{ .SLLI = .{ .rd = GRf(i.i_1.rd), .rs1 = GRf(i.i_1.rs1), .shamt = i.i_1.shamt } },
                        0b101 => switch (i.i_1.op) {
                            0 => .{ .SRLI = .{ .rd = GRf(i.i_1.rd), .rs1 = GRf(i.i_1.rs1), .shamt = i.i_1.shamt } },
                            1 << 4 => .{ .SRAI = .{ .rd = GRf(i.i_1.rd), .rs1 = GRf(i.i_1.rs1), .shamt = i.i_1.shamt } },
                            else => .{ .UNKNOWN_X32 = x32 },
                        },
                    },
                    0b0011011 => switch (i.i.funct3) {
                        0b000 => .{ .ADDIW = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .imm = i.i.imm_11_0 } },
                        0b001 => .{ .SLLIW = .{ .rd = GRf(i.i_1.rd), .rs1 = GRf(i.i_1.rs1), .shamt = i.i_1.shamt } },
                        0b101 => switch (i.i_1.op) {
                            0 => .{ .SRLIW = .{ .rd = GRf(i.i_1.rd), .rs1 = GRf(i.i_1.rs1), .shamt = i.i_1.shamt } },
                            1 << 4 => .{ .SRAIW = .{ .rd = GRf(i.i_1.rd), .rs1 = GRf(i.i_1.rs1), .shamt = i.i_1.shamt } },
                            else => .{ .UNKNOWN_X32 = x32 },
                        },
                        else => .{ .UNKNOWN_X32 = x32 },
                    },
                    0b0110011 => switch (i.r.funct3) {
                        0b000 => switch (i.r.funct7) {
                            0 => .{ .ADD = .{ .rd = GRf(i.r.rd), .rs1 = GRf(i.r.rs1), .rs2 = GRf(i.r.rs2) } },
                            1 << 5 => .{ .SUB = .{ .rd = GRf(i.r.rd), .rs1 = GRf(i.r.rs1), .rs2 = GRf(i.r.rs2) } },
                            else => .{ .UNKNOWN_X32 = x32 },
                        },
                        0b001 => .{ .SLL = .{ .rd = GRf(i.r.rd), .rs1 = GRf(i.r.rs1), .rs2 = GRf(i.r.rs2) } },
                        0b010 => .{ .SLT = .{ .rd = GRf(i.r.rd), .rs1 = GRf(i.r.rs1), .rs2 = GRf(i.r.rs2) } },
                        0b011 => .{ .SLTU = .{ .rd = GRf(i.r.rd), .rs1 = GRf(i.r.rs1), .rs2 = GRf(i.r.rs2) } },
                        0b100 => .{ .XOR = .{ .rd = GRf(i.r.rd), .rs1 = GRf(i.r.rs1), .rs2 = GRf(i.r.rs2) } },
                        0b101 => switch (i.r.funct7) {
                            0 => .{ .SRL = .{ .rd = GRf(i.r.rd), .rs1 = GRf(i.r.rs1), .rs2 = GRf(i.r.rs2) } },
                            1 << 5 => .{ .SRA = .{ .rd = GRf(i.r.rd), .rs1 = GRf(i.r.rs1), .rs2 = GRf(i.r.rs2) } },
                            else => .{ .UNKNOWN_X32 = x32 },
                        },
                        0b110 => .{ .OR = .{ .rd = GRf(i.r.rd), .rs1 = GRf(i.r.rs1), .rs2 = GRf(i.r.rs2) } },
                        0b111 => .{ .AND = .{ .rd = GRf(i.r.rd), .rs1 = GRf(i.r.rs1), .rs2 = GRf(i.r.rs2) } },
                    },
                    0b0111011 => switch (i.r.funct3) {
                        0b000 => switch (i.r.funct7) {
                            0 => .{ .ADDW = .{ .rd = GRf(i.r.rd), .rs1 = GRf(i.r.rs1), .rs2 = GRf(i.r.rs2) } },
                            1 << 5 => .{ .SUBW = .{ .rd = GRf(i.r.rd), .rs1 = GRf(i.r.rs1), .rs2 = GRf(i.r.rs2) } },
                            else => .{ .UNKNOWN_X32 = x32 },
                        },
                        0b001 => .{ .SLLW = .{ .rd = GRf(i.r.rd), .rs1 = GRf(i.r.rs1), .rs2 = GRf(i.r.rs2) } },
                        0b101 => switch (i.r.funct7) {
                            0 => .{ .SRLW = .{ .rd = GRf(i.r.rd), .rs1 = GRf(i.r.rs1), .rs2 = GRf(i.r.rs2) } },
                            1 << 5 => .{ .SRAW = .{ .rd = GRf(i.r.rd), .rs1 = GRf(i.r.rs1), .rs2 = GRf(i.r.rs2) } },
                            else => .{ .UNKNOWN_X32 = x32 },
                        },
                        else => .{ .UNKNOWN_X32 = x32 },
                    },
                    0b0001111 => switch (i.f.func3) {
                        0b000 => .{ .FENCE = .{ .rd = GRf(i.f.rd), .rs1 = GRf(i.f.rs1), .succ = i.f.succ, .pred = i.f.pred, .fm = i.f.fm } },
                        else => .{ .UNKNOWN_X32 = x32 },
                    },
                    0b1110011 => switch (i.i.funct3) {
                        0b000 => switch (i.i.imm_11_0) {
                            0 => .ECALL,
                            1 => .EBREAK,
                            else => .{ .UNKNOWN_X32 = x32 },
                        },
                        0b001 => .{ .CSRRW = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .csr = @bitCast(i.i.imm_11_0) } },
                        0b010 => .{ .CSRRS = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .csr = @bitCast(i.i.imm_11_0) } },
                        0b011 => .{ .CSRRC = .{ .rd = GRf(i.i.rd), .rs1 = GRf(i.i.rs1), .csr = @bitCast(i.i.imm_11_0) } },
                        0b101 => .{ .CSRRWI = .{ .rd = GRf(i.i.rd), .uimm = i.i.rs1, .csr = @bitCast(i.i.imm_11_0) } },
                        0b110 => .{ .CSRRSI = .{ .rd = GRf(i.i.rd), .uimm = i.i.rs1, .csr = @bitCast(i.i.imm_11_0) } },
                        0b111 => .{ .CSRRCI = .{ .rd = GRf(i.i.rd), .uimm = i.i.rs1, .csr = @bitCast(i.i.imm_11_0) } },
                        else => .{ .UNKNOWN_X32 = x32 },
                    },
                    else => .{ .UNKNOWN_X32 = x32 },
                };
            },
            .x64 => |x64| .{ .UNKNOWN_X64 = x64 },
        };
    }

    pub fn len(self: Self) usize {
        return switch (self) {
            .UNKNOWN_X16 => 2,
            .UNKNOWN_X32,
            // RV32I
            .LUI,
            .AUIPC,
            .JAL,
            .JALR,
            .BEQ,
            .BNE,
            .BLT,
            .BGE,
            .BLTU,
            .BGEU,
            .LB,
            .LH,
            .LW,
            .LBU,
            .LHU,
            .SB,
            .SH,
            .SW,
            .ADDI,
            .SLTI,
            .SLTIU,
            .XORI,
            .ORI,
            .ANDI,
            .SLLI,
            .SRLI,
            .SRAI,
            .ADD,
            .SUB,
            .SLL,
            .SLT,
            .SLTU,
            .XOR,
            .SRL,
            .SRA,
            .OR,
            .AND,
            .FENCE,
            .ECALL,
            .EBREAK,
            // RV64I
            .LWU,
            .LD,
            .SD,
            .ADDIW,
            .SLLIW,
            .SRLIW,
            .SRAIW,
            .ADDW,
            .SUBW,
            .SLLW,
            .SRLW,
            .SRAW,
            // Zicsr
            .CSRRW,
            .CSRRS,
            .CSRRC,
            .CSRRWI,
            .CSRRSI,
            .CSRRCI,
            => 4,
            .UNKNOWN_X64 => 8,
        };
    }

    pub fn write(self: Self, writer: std.io.AnyWriter) !void {
        return switch (self) {
            // f RV32I
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
            .ECALL => writer.print("ECALL\n", .{}),
            .EBREAK => writer.print("EBREAK\n", .{}),

            // f RV64I
            .LWU => |i| writer.print("LWU {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), @as(u12, @bitCast(i.offset)) }),
            .LD => |i| writer.print("LD {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), @as(u12, @bitCast(i.offset)) }),
            .SD => |i| writer.print("SD {s}, {s}, 0x{x}\n", .{ i.rs1.name(), i.rs2.name(), @as(u12, @bitCast(i.offset)) }),
            .ADDIW => |i| writer.print("ADDIW {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.imm }),
            .SLLIW => |i| writer.print("SLLIW {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.shamt }),
            .SRLIW => |i| writer.print("SRLIW {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.shamt }),
            .SRAIW => |i| writer.print("SRAIW {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.shamt }),
            .ADDW => |i| writer.print("ADDW {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .SUBW => |i| writer.print("SUBW {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .SLLW => |i| writer.print("SLLW {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .SRLW => |i| writer.print("SRL {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),
            .SRAW => |i| writer.print("SRA {s}, {s}, {s}\n", .{ i.rd.name(), i.rs1.name(), i.rs2.name() }),

            // f Zicsr
            .CSRRW => |i| writer.print("CSRRW {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.csr }),
            .CSRRS => |i| writer.print("CSRRS {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.csr }),
            .CSRRC => |i| writer.print("CSRRC {s}, {s}, 0x{x}\n", .{ i.rd.name(), i.rs1.name(), i.csr }),
            .CSRRWI => |i| writer.print("CSRRWI {s}, 0x{x}, 0x{x}\n", .{ i.rd.name(), i.uimm, i.csr }),
            .CSRRSI => |i| writer.print("CSRRSI {s}, 0x{x}, 0x{x}\n", .{ i.rd.name(), i.uimm, i.csr }),
            .CSRRCI => |i| writer.print("CSRRCI {s}, 0x{x}, 0x{x}\n", .{ i.rd.name(), i.uimm, i.csr }),

            .UNKNOWN_X16 => |data| writer.print("DB 0x{x}\n", .{data}),
            .UNKNOWN_X32 => |data| writer.print("DB 0x{x}\n", .{data}),
            .UNKNOWN_X64 => |data| writer.print("DB 0x{x}\n", .{data}),
        };
    }

    pub fn used_grs(self: Self) [3]GR {
        return switch (self) {
            // RV32I
            .LUI => |i| .{ i.rd, GR.ZERO, GR.ZERO },
            .AUIPC => |i| .{ i.rd, GR.ZERO, GR.ZERO },
            .JAL => |i| .{ i.rd, GR.ZERO, GR.ZERO },
            .JALR => |i| .{ i.rd, i.rs1, GR.ZERO },
            .BEQ => |i| .{ i.rs1, i.rs2, GR.ZERO },
            .BNE => |i| .{ i.rs1, i.rs2, GR.ZERO },
            .BLT => |i| .{ i.rs1, i.rs2, GR.ZERO },
            .BGE => |i| .{ i.rs1, i.rs2, GR.ZERO },
            .BLTU => |i| .{ i.rs1, i.rs2, GR.ZERO },
            .BGEU => |i| .{ i.rs1, i.rs2, GR.ZERO },
            .LB => |i| .{ i.rd, i.rs1, GR.ZERO },
            .LH => |i| .{ i.rd, i.rs1, GR.ZERO },
            .LW => |i| .{ i.rd, i.rs1, GR.ZERO },
            .LBU => |i| .{ i.rd, i.rs1, GR.ZERO },
            .LHU => |i| .{ i.rd, i.rs1, GR.ZERO },
            .SB => |i| .{ i.rs1, i.rs2, GR.ZERO },
            .SH => |i| .{ i.rs1, i.rs2, GR.ZERO },
            .SW => |i| .{ i.rs1, i.rs2, GR.ZERO },
            .ADDI => |i| .{ i.rd, i.rs1, GR.ZERO },
            .SLTI => |i| .{ i.rd, i.rs1, GR.ZERO },
            .SLTIU => |i| .{ i.rd, i.rs1, GR.ZERO },
            .XORI => |i| .{ i.rd, i.rs1, GR.ZERO },
            .ORI => |i| .{ i.rd, i.rs1, GR.ZERO },
            .ANDI => |i| .{ i.rd, i.rs1, GR.ZERO },
            .SLLI => |i| .{ i.rd, i.rs1, GR.ZERO },
            .SRLI => |i| .{ i.rd, i.rs1, GR.ZERO },
            .SRAI => |i| .{ i.rd, i.rs1, GR.ZERO },
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
            .FENCE => |_| .{ GR.ZERO, GR.ZERO, GR.ZERO },
            .ECALL => .{ GR.ZERO, GR.ZERO, GR.ZERO },
            .EBREAK => .{ GR.ZERO, GR.ZERO, GR.ZERO },

            // RV64I

            .LWU => |i| .{ i.rd, i.rs1, GR.ZERO },
            .LD => |i| .{ i.rd, i.rs1, GR.ZERO },
            .SD => |i| .{ i.rs1, i.rs2, GR.ZERO },
            .ADDIW => |i| .{ i.rd, i.rs1, GR.ZERO },
            .SLLIW => |i| .{ i.rd, i.rs1, GR.ZERO },
            .SRLIW => |i| .{ i.rd, i.rs1, GR.ZERO },
            .SRAIW => |i| .{ i.rd, i.rs1, GR.ZERO },
            .ADDW => |i| .{ i.rd, i.rs1, i.rs2 },
            .SUBW => |i| .{ i.rd, i.rs1, i.rs2 },
            .SLLW => |i| .{ i.rd, i.rs1, i.rs2 },
            .SRLW => |i| .{ i.rd, i.rs1, i.rs2 },
            .SRAW => |i| .{ i.rd, i.rs1, i.rs2 },

            // Ziscr

            .CSRRW => |i| .{ i.rd, i.rs1, GR.ZERO },
            .CSRRS => |i| .{ i.rd, i.rs1, GR.ZERO },
            .CSRRC => |i| .{ i.rd, i.rs1, GR.ZERO },
            .CSRRWI => |i| .{ i.rd, GR.ZERO, GR.ZERO },
            .CSRRSI => |i| .{ i.rd, GR.ZERO, GR.ZERO },
            .CSRRCI => |i| .{ i.rd, GR.ZERO, GR.ZERO },

            .UNKNOWN_X16 => |_| .{ GR.ZERO, GR.ZERO, GR.ZERO },
            .UNKNOWN_X32 => |_| .{ GR.ZERO, GR.ZERO, GR.ZERO },
            .UNKNOWN_X64 => |_| .{ GR.ZERO, GR.ZERO, GR.ZERO },
        };
    }

    // a RV32I

    pub fn lui(rd: GR, imm: u20) Self {
        return Self{ .LUI = .{ .rd = rd, .imm = imm } };
    }
    pub fn auipc(rd: GR, imm: u20) Self {
        return Self{ .AUIPC = .{ .rd = rd, .imm = imm } };
    }
    pub fn jal(rd: GR, imm: i20) Self {
        return Self{ .JAL = .{ .rd = rd, .imm = imm } };
    }
    pub fn jalr(rd: GR, rs1: GR, imm: i12) Self {
        return Self{ .JALR = .{ .rd = rd, .rs1 = rs1, .imm = imm } };
    }
    pub fn beq(rs1: GR, rs2: GR, offset: i12) Self {
        return Self{ .BEQ = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn bne(rs1: GR, rs2: GR, offset: i12) Self {
        return Self{ .BNE = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn blt(rs1: GR, rs2: GR, offset: i12) Self {
        return Self{ .BLT = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn bge(rs1: GR, rs2: GR, offset: i12) Self {
        return Self{ .BGE = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn bltu(rs1: GR, rs2: GR, offset: i12) Self {
        return Self{ .BLTU = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn bgeu(rs1: GR, rs2: GR, offset: i12) Self {
        return Self{ .BGEU = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }

    pub fn lb(rd: GR, rs1: GR, offset: i12) Self {
        return Self{ .LB = .{ .rd = rd, .rs1 = rs1, .offset = offset } };
    }
    pub fn lh(rd: GR, rs1: GR, offset: i12) Self {
        return Self{ .LH = .{ .rd = rd, .rs1 = rs1, .offset = offset } };
    }
    pub fn lw(rd: GR, rs1: GR, offset: i12) Self {
        return Self{ .LW = .{ .rd = rd, .rs1 = rs1, .offset = offset } };
    }
    pub fn lbu(rd: GR, rs1: GR, offset: i12) Self {
        return Self{ .LBU = .{ .rd = rd, .rs1 = rs1, .offset = offset } };
    }
    pub fn lhu(rd: GR, rs1: GR, offset: i12) Self {
        return Self{ .LHU = .{ .rd = rd, .rs1 = rs1, .offset = offset } };
    }

    pub fn sb(rs1: GR, rs2: GR, offset: i12) Self {
        return Self{ .SB = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn sh(rs1: GR, rs2: GR, offset: i12) Self {
        return Self{ .SH = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }
    pub fn sw(rs1: GR, rs2: GR, offset: i12) Self {
        return Self{ .SW = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }

    pub fn addi(rd: GR, rs1: GR, imm: i12) Self {
        return Self{ .ADDI = .{ .rd = rd, .rs1 = rs1, .imm = imm } };
    }
    pub fn slti(rd: GR, rs1: GR, imm: i12) Self {
        return Self{ .SLTI = .{ .rd = rd, .rs1 = rs1, .imm = imm } };
    }
    pub fn sltiu(rd: GR, rs1: GR, imm: i12) Self {
        return Self{ .SLTIU = .{ .rd = rd, .rs1 = rs1, .imm = imm } };
    }
    pub fn xori(rd: GR, rs1: GR, imm: i12) Self {
        return Self{ .XORI = .{ .rd = rd, .rs1 = rs1, .imm = imm } };
    }
    pub fn ori(rd: GR, rs1: GR, imm: i12) Self {
        return Self{ .ORI = .{ .rd = rd, .rs1 = rs1, .imm = imm } };
    }
    pub fn andi(rd: GR, rs1: GR, imm: i12) Self {
        return Self{ .ANDI = .{ .rd = rd, .rs1 = rs1, .imm = imm } };
    }
    pub fn slli(rd: GR, rs1: GR, shamt: u6) Self {
        return Self{ .SLLI = .{ .rd = rd, .rs1 = rs1, .shamt = shamt } };
    }
    pub fn srli(rd: GR, rs1: GR, shamt: u6) Self {
        return Self{ .SRLI = .{ .rd = rd, .rs1 = rs1, .shamt = shamt } };
    }
    pub fn srai(rd: GR, rs1: GR, shamt: u6) Self {
        return Self{ .SRAI = .{ .rd = rd, .rs1 = rs1, .shamt = shamt } };
    }
    pub fn add(rd: GR, rs1: GR, rs2: GR) Self {
        return Self{ .ADD = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn sub(rd: GR, rs1: GR, rs2: GR) Self {
        return Self{ .SUB = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn sll(rd: GR, rs1: GR, rs2: GR) Self {
        return Self{ .SLL = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn slt(rd: GR, rs1: GR, rs2: GR) Self {
        return Self{ .SLT = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn sltu(rd: GR, rs1: GR, rs2: GR) Self {
        return Self{ .SLTU = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn xor(rd: GR, rs1: GR, rs2: GR) Self {
        return Self{ .XOR = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn srl(rd: GR, rs1: GR, rs2: GR) Self {
        return Self{ .SRL = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn sra(rd: GR, rs1: GR, rs2: GR) Self {
        return Self{ .SRA = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn _or(rd: GR, rs1: GR, rs2: GR) Self {
        return Self{ .ORI = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn _and(rd: GR, rs1: GR, rs2: GR) Self {
        return Self{ .AND = .{ .rd = rd, .rs1 = rs1, .rs2 = rs2 } };
    }
    pub fn fence(rd: GR, rs1: GR, succ: base.FFlags, pred: base.FFlags, fm: u4) Self {
        return Self{ .FENCE = .{ .rd = rd, .rs1 = rs1, .succ = succ, .pred = pred, .fm = fm } };
    }
    pub fn ecall() Self {
        return Self{.ECALL};
    }
    pub fn ebreak() Self {
        return Self{.EBREAK};
    }

    // a RV64I

    pub fn lwu(rd: GR, rs1: GR, offset: i12) Self {
        return Self{ .LWU = .{ .rd = rd, .rs1 = rs1, .offset = offset } };
    }

    pub fn ld(rd: GR, rs1: GR, offset: i12) Self {
        return Self{ .LD = .{ .rd = rd, .rs1 = rs1, .offset = offset } };
    }

    pub fn sd(rs1: GR, rs2: GR, offset: i12) Self {
        return Self{ .SD = .{ .rs1 = rs1, .rs2 = rs2, .offset = offset } };
    }

    // a Ziscr

    pub fn csrrw(rd: GR, rs1: GR, csr: u12) Self {
        return Self{ .CSRRW = .{ .rd = rd, .rs1 = rs1, .csr = csr } };
    }
    pub fn csrrs(rd: GR, rs1: GR, csr: u12) Self {
        return Self{ .CSRRS = .{ .rd = rd, .rs1 = rs1, .csr = csr } };
    }
    pub fn csrrc(rd: GR, rs1: GR, csr: u12) Self {
        return Self{ .CSRRC = .{ .rd = rd, .rs1 = rs1, .csr = csr } };
    }
    pub fn csrrwi(rd: GR, uimm: u5, csr: u12) Self {
        return Self{ .CSRRWI = .{ .rd = rd, .uimm = uimm, .csr = csr } };
    }
    pub fn csrrsi(rd: GR, uimm: u5, csr: u12) Self {
        return Self{ .CSRRSI = .{ .rd = rd, .uimm = uimm, .csr = csr } };
    }
    pub fn csrrci(rd: GR, uimm: u5, csr: u12) Self {
        return Self{ .CSRRCI = .{ .rd = rd, .uimm = uimm, .csr = csr } };
    }
};
