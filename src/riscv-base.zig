const std = @import("std");

pub const GeneralRegsNames = [_][]const u8{ "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "fp", "s1", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6" };
pub const GeneralReg = enum(u5) {
    ZERO,
    RA,
    SP,
    GP,
    TP,
    T0,
    T1,
    T2,
    FP,
    S1,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,
    T3,
    T4,
    T5,
    T6,

    pub fn to_u5(self: @This()) u5 {
        return @intFromEnum(self);
    }

    pub fn from_u5(operand: u5) @This() {
        return @enumFromInt(operand);
    }

    pub fn name(self: @This()) []const u8 {
        return GeneralRegsNames[self.to_u5()];
    }
};
pub const VarInstr = union(enum) {
    x16: u16,
    x32: u32,
    x64: u64,

    const Self = @This();

    pub fn from_memory(memory: []const u8) error{ EndOfStream, VarInstrNotImplemented }!Self {
        if (memory.len < 2) return error.EndOfStream;

        const byte = memory[0];
        if (byte & 0b1111111 == 0b1111111) {
            std.log.err("64-bit+ instruction not supported", .{});
            return error.VarInstrNotImplemented;
        } else if (byte & 0b1111111 == 0b0111111) {
            if (memory.len < 8) return error.EndOfStream;
            const x64 = std.mem.readInt(u64, memory[0..8], .little);
            return .{ .x64 = x64 };
        } else if (byte & 0b111111 == 0b011111) {
            std.log.err("48-bit instruction not supported", .{});
            return error.VarInstrNotImplemented;
        } else if (byte & 0b11 == 0b11) {
            if (memory.len < 4) return error.EndOfStream;
            const x32 = std.mem.readInt(u32, memory[0..4], .little);
            return .{ .x32 = x32 };
        }
        const x16 = std.mem.readInt(u16, memory[0..2], .little);
        return .{ .x16 = x16 };
    }

    pub fn to_memory(self: Self, memory: []u8) error{OutOfSpace}!usize {
        switch (self) {
            .x16 => |v| {
                if (memory.len < 2) {
                    return error.OutOfSpace;
                }
                std.mem.writeInt(u16, memory[0..2], v, .little);
                return 2;
            },
            .x32 => |v| {
                if (memory.len < 4) {
                    return error.OutOfSpace;
                }
                std.mem.writeInt(u32, memory[0..4], v, .little);
                return 4;
            },
            .x64 => |v| {
                if (memory.len < 8) {
                    return error.OutOfSpace;
                }
                std.mem.writeInt(u64, memory[0..8], v, .little);
                return 8;
            },
        }
    }

    pub fn X16(data: u16) Self {
        return .{ .x16 = data };
    }

    pub fn X32(data: u32) Self {
        return .{ .x32 = data };
    }

    pub fn X64(data: u64) Self {
        return .{ .x64 = data };
    }
};

pub const FFlags = packed struct {
    write: bool,
    read: bool,
    out: bool,
    input: bool,

    pub fn from_u4(value: u4) @This() {
        return @bitCast(value);
    }

    pub fn to_u4(self: @This()) u4 {
        return @bitCast(self);
    }

    pub fn name(self: @This()) []const u8 {
        return switch (self.to_u4()) {
            0 => "    ",
            1 => "   W",
            2 => "   R",
            3 => "  WR",
            4 => "   O",
            5 => "  WO",
            6 => "  RO",
            7 => " WRO",
            8 => "   I",
            9 => "  WI",
            10 => "  RI",
            11 => " WRI",
            12 => "  OI",
            13 => " WOI",
            14 => " ROI",
            15 => "WROI",
        };
    }
};

pub const ImmediateVariantX32 = packed union {
    pub const R = packed struct { opcode: u7, rd: u5, funct3: u3, rs1: u5, rs2: u5, funct7: u7 };
    pub const I = packed struct { opcode: u7, rd: u5, funct3: u3, rs1: u5, imm_11_0: i12 };
    pub const I_1 = packed struct { opcode: u7, rd: u5, funct3: u3, rs1: u5, shamt: u6, op: u6 };
    pub const S = packed struct {
        opcode: u7,
        imm_4_0: u5,
        funct3: u3,
        rs1: u5,
        rs2: u5,
        imm_11_5: u7,

        pub fn get_imm(self: @This()) i12 {
            return @bitCast((@as(u12, self.imm_11_5) << 5) + @as(u12, self.imm_4_0));
        }
    };
    pub const B = packed struct {
        opcode: u7,
        imm_11: u1,
        imm_4_1: u4,
        funct3: u3,
        rs1: u5,
        rs2: u5,
        imm_10_5: u6,
        imm_12: u1,

        pub fn get_imm(self: @This()) i12 {
            return @bitCast((@as(u12, self.imm_12) << 11) + (@as(u12, self.imm_11) << 10) + (@as(u12, self.imm_10_5) << 4) + @as(u12, self.imm_4_1));
        }
    };
    pub const U = packed struct { opcode: u7, rd: u5, imm_31_12: u20 };
    pub const J = packed struct {
        opcode: u7,
        rd: u5,
        imm_19_12: u8,
        b_11: u1,
        imm_10_1: u10,
        b_20: u1,

        pub fn get_imm(self: @This()) i20 {
            return @bitCast((@as(u20, self.b_20) << 19) + (@as(u20, self.b_11) << 10) + (@as(u20, self.imm_19_12) << 11) + @as(u20, self.imm_10_1));
        }
    };
    pub const F = packed struct { opcode: u7, rd: u5, func3: u3, rs1: u5, succ: FFlags, pred: FFlags, fm: u4 };

    opcode: u7,
    r: R,
    i: I,
    i_1: I_1,
    s: S,
    b: B,
    u: U,
    j: J,
    f: F,

    const Self = @This();

    pub fn from_u32(value: u32) Self {
        return @bitCast(value);
    }

    pub fn to_u32(self: Self) u32 {
        return @bitCast(self);
    }

    pub fn to_varinstr(self: Self) VarInstr {
        return .{ .x32 = self.to_u32() };
    }
};
