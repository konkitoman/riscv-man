const std = @import("std");

pub const IntRegNames = [_][]const u8{ "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "fp", "s1", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6" };
pub const IntReg = enum(u5) {
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
        return IntRegNames[self.to_u5()];
    }

    pub fn to_popular(self: @This()) ?PopularIntReg {
        const i = self.to_u5();
        if (i <= 8 or i >= 15) {
            return null;
        }

        return PopularIntReg.from_u3(@truncate(i - 8));
    }
};

pub const PopularIntReg = enum(u3) {
    S0,
    S1,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,

    pub fn to_u3(self: @This()) u3 {
        return @intFromEnum(self);
    }

    pub fn from_u3(operand: u3) @This() {
        return @enumFromInt(operand);
    }

    pub fn to_reg(self: @This()) IntReg {
        return IntReg.from_u5(@as(u5, self.to_u3()) + 8);
    }

    pub fn name(self: @This()) []const u8 {
        return IntRegNames[self.to_reg().to_u5()];
    }
};

pub const Arch = enum {
    X32,
    X64,

    pub fn uarch(self: @This()) type {
        return switch (self) {
            .X32 => u32,
            .X64 => u64,
        };
    }

    pub fn iarch(self: @This()) type {
        return switch (self) {
            .X32 => i32,
            .X64 => i64,
        };
    }
};

pub const CSRAddr = enum(u12) {
    // RISC-V unprivileged

    fflags = 0x001,
    frm = 0x002,
    fcsr = 0x003,
    cycle = 0xC00,
    time = 0xC01,
    instret = 0xC02,
    hpmcounter3 = 0xC03,
    hpmcounter4 = 0xC04,
    hpmcounter5 = 0xC05,
    hpmcounter6 = 0xC06,
    hpmcounter7 = 0xC07,
    hpmcounter8 = 0xC08,
    hpmcounter9 = 0xC09,
    hpmcounter10 = 0xC0A,
    hpmcounter11 = 0xC0B,
    hpmcounter12 = 0xC0C,
    hpmcounter13 = 0xC0D,
    hpmcounter14 = 0xC0E,
    hpmcounter15 = 0xC0F,
    hpmcounter16 = 0xC10,
    hpmcounter17 = 0xC11,
    hpmcounter18 = 0xC12,
    hpmcounter19 = 0xC13,
    hpmcounter20 = 0xC14,
    hpmcounter21 = 0xC15,
    hpmcounter22 = 0xC16,
    hpmcounter23 = 0xC17,
    hpmcounter24 = 0xC18,
    hpmcounter25 = 0xC19,
    hpmcounter26 = 0xC1A,
    hpmcounter27 = 0xC1B,
    hpmcounter28 = 0xC1C,
    hpmcounter29 = 0xC1D,
    hpmcounter30 = 0xC1E,
    hpmcounter31 = 0xC1F,

    // RV32 only
    cycleh = 0xC80,
    timeh = 0xC81,
    instreth = 0xC82,
    hpmcounter3h = 0xC83,
    hpmcounter4h = 0xC84,
    hpmcounter5h = 0xC85,
    hpmcounter6h = 0xC86,
    hpmcounter7h = 0xC87,
    hpmcounter8h = 0xC88,
    hpmcounter9h = 0xC89,
    hpmcounter10h = 0xC8A,
    hpmcounter11h = 0xC8B,
    hpmcounter12h = 0xC8C,
    hpmcounter13h = 0xC8D,
    hpmcounter14h = 0xC8E,
    hpmcounter15h = 0xC8F,
    hpmcounter16h = 0xC90,
    hpmcounter17h = 0xC91,
    hpmcounter18h = 0xC92,
    hpmcounter19h = 0xC93,
    hpmcounter20h = 0xC94,
    hpmcounter21h = 0xC95,
    hpmcounter22h = 0xC96,
    hpmcounter23h = 0xC97,
    hpmcounter24h = 0xC98,
    hpmcounter25h = 0xC99,
    hpmcounter26h = 0xC9A,
    hpmcounter27h = 0xC9B,
    hpmcounter28h = 0xC9C,
    hpmcounter29h = 0xC9D,
    hpmcounter30h = 0xC9E,
    hpmcounter31h = 0xC9F,

    // RISC-V supervisor-level

    sstatus = 0x100,
    sie = 0x104,
    stvec = 0x105,
    scountern = 0x106,

    senvcfg = 0x10A,

    scountinhibit = 0x120,

    sscratch = 0x140,
    sepc = 0x141,
    scause = 0x142,
    stval = 0x143,
    sip = 0x144,
    scountovf = 0xDA0,

    stap = 0x180,

    scontext = 0x5A8,

    sstatenn0 = 0x10C,
    sstatenn1 = 0x10D,
    sstatenn2 = 0x10E,
    sstatenn3 = 0x10F,

    // RISC-V hypervisor

    hstatus = 0x600,
    hedeleg = 0x602,
    hideleg = 0x603,
    hie = 0x604,
    hcounteren = 0x606,
    hgeie = 0x607,
    htval = 0x642,
    hip = 0x644,
    hvip = 0x645,
    htinst = 0x64A,
    hgeip = 0xE12,
    henvcfg = 0x60A,

    // RV32 only
    henvcfgh = 0x61A,

    hgatp = 0x680,
    hcontext = 0x6A8,
    htimedelta = 0x605,

    // RV32 only
    htimedeltah = 0x615,

    hstateen0 = 0x60C,
    hstateen1 = 0x60D,
    hstateen2 = 0x60E,
    hstateen3 = 0x60F,

    // RV32 only
    hstateen0h = 0x61C,
    hstateen1h = 0x61D,
    hstateen2h = 0x61E,
    hstateen3h = 0x61F,

    vsstatus = 0x200,
    vsie = 0x204,
    vstvec = 0x205,
    vsscratch = 0x240,
    vsepc = 0x241,
    vscause = 0x242,
    vstval = 0x243,
    vsip = 0x244,
    vsatp = 0x280,

    // RISC-V machine-level

    mvendorid = 0xF11,
    marchid = 0xF12,
    mimpid = 0xF13,
    mhartid = 0xF14,
    mconfigptr = 0xF15,
    mstatus = 0x300,
    misa = 0x301,
    medeleg = 0x302,
    mideleg = 0x303,
    mie = 0x304,
    mtvec = 0x305,
    mcounteren = 0x306,

    // RV32 only
    mstatush = 0x310,
    medelegh = 0x312,

    mscratch = 0x340,
    mepc = 0x341,
    mcause = 0x342,
    mtval = 0x343,
    mip = 0x344,
    mtinst = 0x34A,
    mtval2 = 0x34B,
    menvcfg = 0x30A,
    mseccfg = 0x747,

    // RV32 only
    menvcfgh = 0x31A,
    mseccfgh = 0x757,

    pmpcfg0 = 0x3A0,
    pmpcfg2 = 0x3A2,
    pmpcfg4 = 0x3A4,
    pmpcfg6 = 0x3A6,
    pmpcfg8 = 0x3A8,
    pmpcfg10 = 0x3AA,
    pmpcfg12 = 0x3AC,
    pmpcfg14 = 0x3AE,

    // RV32 only
    pmpcfg1 = 0x3A1,
    pmpcfg3 = 0x3A3,
    pmpcfg5 = 0x3A5,
    pmpcfg7 = 0x3A7,
    pmpcfg9 = 0x3A9,
    pmpcfg11 = 0x3AB,
    pmpcfg13 = 0x3AD,
    pmpcfg15 = 0x3AF,

    pmpaddr0 = 0x3B0,
    pmpaddr1 = 0x3B1,
    pmpaddr2 = 0x3B2,
    pmpaddr3 = 0x3B3,
    pmpaddr4 = 0x3B4,
    pmpaddr5 = 0x3B5,
    pmpaddr6 = 0x3B6,
    pmpaddr7 = 0x3B7,
    pmpaddr8 = 0x3B8,
    pmpaddr9 = 0x3B9,
    pmpaddr10 = 0x3BA,
    pmpaddr11 = 0x3BB,
    pmpaddr12 = 0x3BC,
    pmpaddr13 = 0x3BD,
    pmpaddr14 = 0x3BE,
    pmpaddr15 = 0x3BF,
    pmpaddr16 = 0x3C0,
    pmpaddr17 = 0x3C1,
    pmpaddr18 = 0x3C2,
    pmpaddr19 = 0x3C3,
    pmpaddr20 = 0x3C4,
    pmpaddr21 = 0x3C5,
    pmpaddr22 = 0x3C6,
    pmpaddr23 = 0x3C7,
    pmpaddr24 = 0x3C8,
    pmpaddr25 = 0x3C9,
    pmpaddr26 = 0x3CA,
    pmpaddr27 = 0x3CB,
    pmpaddr28 = 0x3CC,
    pmpaddr29 = 0x3CD,
    pmpaddr30 = 0x3CE,
    pmpaddr31 = 0x3CF,
    pmpaddr32 = 0x3D0,
    pmpaddr33 = 0x3D1,
    pmpaddr34 = 0x3D2,
    pmpaddr35 = 0x3D3,
    pmpaddr36 = 0x3D4,
    pmpaddr37 = 0x3D5,
    pmpaddr38 = 0x3D6,
    pmpaddr39 = 0x3D7,
    pmpaddr40 = 0x3D8,
    pmpaddr41 = 0x3D9,
    pmpaddr42 = 0x3DA,
    pmpaddr43 = 0x3DB,
    pmpaddr44 = 0x3DC,
    pmpaddr45 = 0x3DD,
    pmpaddr46 = 0x3DE,
    pmpaddr47 = 0x3DF,
    pmpaddr48 = 0x3E0,
    pmpaddr49 = 0x3E1,
    pmpaddr50 = 0x3E2,
    pmpaddr51 = 0x3E3,
    pmpaddr52 = 0x3E4,
    pmpaddr53 = 0x3E5,
    pmpaddr54 = 0x3E6,
    pmpaddr55 = 0x3E7,
    pmpaddr56 = 0x3E8,
    pmpaddr57 = 0x3E9,
    pmpaddr58 = 0x3EA,
    pmpaddr59 = 0x3EB,
    pmpaddr60 = 0x3EC,
    pmpaddr61 = 0x3ED,
    pmpaddr62 = 0x3EE,
    pmpaddr63 = 0x3EF,
    mstateen0 = 0x30C,
    mstateen1 = 0x30D,
    mstateen2 = 0x30E,
    mstateen3 = 0x30F,
    mstateen0h = 0x31C,
    mstateen1h = 0x31D,
    mstateen2h = 0x31E,
    mstateen3h = 0x31F,
    mnscratch = 0x740,
    mnepc = 0x741,
    mncause = 0x742,
    mnstatus = 0x744,
    mcycle = 0xB00,
    minstret = 0xB02,
    mhpmcounter3 = 0xB03,
    mhpmcounter4 = 0xB04,
    mhpmcounter5 = 0xB05,
    mhpmcounter6 = 0xB06,
    mhpmcounter7 = 0xB07,
    mhpmcounter8 = 0xB08,
    mhpmcounter9 = 0xB09,
    mhpmcounter10 = 0xB0A,
    mhpmcounter11 = 0xB0B,
    mhpmcounter12 = 0xB0C,
    mhpmcounter13 = 0xB0D,
    mhpmcounter14 = 0xB0E,
    mhpmcounter15 = 0xB0F,
    mhpmcounter16 = 0xB10,
    mhpmcounter17 = 0xB11,
    mhpmcounter18 = 0xB12,
    mhpmcounter19 = 0xB13,
    mhpmcounter20 = 0xB14,
    mhpmcounter21 = 0xB15,
    mhpmcounter22 = 0xB16,
    mhpmcounter23 = 0xB17,
    mhpmcounter24 = 0xB18,
    mhpmcounter25 = 0xB19,
    mhpmcounter26 = 0xB1A,
    mhpmcounter27 = 0xB1B,
    mhpmcounter28 = 0xB1C,
    mhpmcounter29 = 0xB1D,
    mhpmcounter30 = 0xB1E,
    mhpmcounter31 = 0xB1F,

    // RV32 only
    mcycleh = 0xB80,
    minstrecth = 0xB82,
    mhpmcounter3h = 0xB83,
    mhpmcounter4h = 0xB84,
    mhpmcounter5h = 0xB85,
    mhpmcounter6h = 0xB86,
    mhpmcounter7h = 0xB87,
    mhpmcounter8h = 0xB88,
    mhpmcounter9h = 0xB89,
    mhpmcounter10h = 0xB8A,
    mhpmcounter11h = 0xB8B,
    mhpmcounter12h = 0xB8C,
    mhpmcounter13h = 0xB8D,
    mhpmcounter14h = 0xB8E,
    mhpmcounter15h = 0xB8F,
    mhpmcounter16h = 0xB90,
    mhpmcounter17h = 0xB91,
    mhpmcounter18h = 0xB92,
    mhpmcounter19h = 0xB93,
    mhpmcounter20h = 0xB94,
    mhpmcounter21h = 0xB95,
    mhpmcounter22h = 0xB96,
    mhpmcounter23h = 0xB97,
    mhpmcounter24h = 0xB98,
    mhpmcounter25h = 0xB99,
    mhpmcounter26h = 0xB9A,
    mhpmcounter27h = 0xB9B,
    mhpmcounter28h = 0xB9C,
    mhpmcounter29h = 0xB9D,
    mhpmcounter30h = 0xB9E,
    mhpmcounter31h = 0xB9F,

    mcountinhibit = 0x320,
    mhpmevent3 = 0x323,
    mhpmevent4 = 0x324,
    mhpmevent5 = 0x325,
    mhpmevent6 = 0x326,
    mhpmevent7 = 0x327,
    mhpmevent8 = 0x328,
    mhpmevent9 = 0x329,
    mhpmevent10 = 0x32A,
    mhpmevent11 = 0x32B,
    mhpmevent12 = 0x32C,
    mhpmevent13 = 0x32D,
    mhpmevent14 = 0x32E,
    mhpmevent15 = 0x32F,
    mhpmevent16 = 0x330,
    mhpmevent17 = 0x331,
    mhpmevent18 = 0x332,
    mhpmevent19 = 0x333,
    mhpmevent20 = 0x334,
    mhpmevent21 = 0x335,
    mhpmevent22 = 0x336,
    mhpmevent23 = 0x337,
    mhpmevent24 = 0x338,
    mhpmevent25 = 0x339,
    mhpmevent26 = 0x33A,
    mhpmevent27 = 0x33B,
    mhpmevent28 = 0x33C,
    mhpmevent29 = 0x33D,
    mhpmevent30 = 0x33E,
    mhpmevent31 = 0x33F,

    // RV32 only
    mhpmevent3h = 0x723,
    mhpmevent4h = 0x724,
    mhpmevent5h = 0x725,
    mhpmevent6h = 0x726,
    mhpmevent7h = 0x727,
    mhpmevent8h = 0x728,
    mhpmevent9h = 0x729,
    mhpmevent10h = 0x72A,
    mhpmevent11h = 0x72B,
    mhpmevent12h = 0x72C,
    mhpmevent13h = 0x72D,
    mhpmevent14h = 0x72E,
    mhpmevent15h = 0x72F,
    mhpmevent16h = 0x710,
    mhpmevent17h = 0x711,
    mhpmevent18h = 0x712,
    mhpmevent19h = 0x713,
    mhpmevent20h = 0x714,
    mhpmevent21h = 0x715,
    mhpmevent22h = 0x716,
    mhpmevent23h = 0x717,
    mhpmevent24h = 0x718,
    mhpmevent25h = 0x719,
    mhpmevent26h = 0x71A,
    mhpmevent27h = 0x71B,
    mhpmevent28h = 0x71C,
    mhpmevent29h = 0x71D,
    mhpmevent30h = 0x71E,
    mhpmevent31h = 0x73F,

    tselect = 0x7A0,
    tdata1 = 0x7A1,
    tdata2 = 0x7A2,
    tdata3 = 0x7A3,
    mcontext = 0x7A8,
    dscr = 0x7B0,
    dpc = 0x7B1,
    dscratch0 = 0x7B2,
    dscratch1 = 0x7B3,

    pub fn to_u12(self: @This()) u12 {
        return @intFromEnum(self);
    }
};

pub const CSRAddrU = union(enum) {
    known: CSRAddr,
    unknown: u12,

    pub fn from_u12(value: u12) @This() {
        if (std.meta.intToEnum(CSRAddr, value)) |known| {
            return .{ .known = known };
        } else {
            return .{ .unknown = value };
        }
    }

    pub fn to_u12(self: @This()) u12 {
        return switch (self) {
            .knwon => |v| v.to_u12(),
            .unknwon => |u| u,
        };
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

    pub fn debug(self: Self) void {
        switch (self) {
            .x16 => |x| {
                std.debug.print("X16 Instr: {b:0>16}\n", .{x});
            },
            .x32 => |x| {
                std.debug.print("X32 Instr: {b:0>32}\n", .{x});
            },
            .x64 => |x| {
                std.debug.print("X64 Instr: {b:0>64}\n", .{x});
            },
        }
    }
};

pub const FFlags = packed struct {
    write: bool = false,
    read: bool = false,
    out: bool = false,
    input: bool = false,

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
            2 => "  R ",
            3 => "  RW",
            4 => " O  ",
            5 => " O W",
            6 => " OR ",
            7 => " ORW",
            8 => "I   ",
            9 => "I  W",
            10 => "I R ",
            11 => "I RW",
            12 => "IO  ",
            13 => "IO W",
            14 => "IOR ",
            15 => "IORW",
        };
    }
};

pub const InstrFormatX32 = packed union {
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

pub const InstrFormatX16 = packed union {
    pub const CR = packed struct { op: u2, rs2: u5, rd: u5, funct4: u4 };
    pub const CI = packed struct { op: u2, imm_2_6: u5, rd: u5, imm_12: u1, funct3: u3 };
    pub const CSS = packed struct { op: u2, rs2: u5, imm: u6, funct3: u3 };
    pub const CIW = packed struct {
        op: u2,
        prd: u3,
        imm: u8,
        funct3: u3,

        pub fn rd(self: @This()) u5 {
            return @as(u5, self.prd) + 8;
        }
    };
    pub const CL = packed struct {
        op: u2,
        prd: u3,
        imm1: u2,
        prs1: u3,
        imm2: u3,
        funct3: u3,

        pub fn rd(self: @This()) u5 {
            return @as(u5, self.prd) + 8;
        }

        pub fn rs1(self: @This()) u5 {
            return @as(u5, self.rs1) + 8;
        }
    };

    pub const CS = packed struct {
        op: u2,
        prs2: u3,
        imm1: u2,
        prs1: u3,
        imm2: u3,
        funct3: u3,

        pub fn rs2(self: @This()) u5 {
            return @as(u5, self.prs2) + 8;
        }

        pub fn rs1(self: @This()) u5 {
            return @as(u5, self.rs1) + 8;
        }
    };

    pub const CA = packed struct {
        op: u2,
        prs2: u3,
        funct2: u2,
        prd: u3,
        offset: u3,
        funct3: u3,

        pub fn rs2(self: @This()) u5 {
            return @as(u5, self.rs2) + 8;
        }

        pub fn rd(self: @This()) u5 {
            return @as(u5, self.prd) + 8;
        }
    };

    pub const CB = packed struct {
        op: u2,
        offset1: u5,
        prd: u3,
        offset2: u3,
        funct3: u3,

        pub fn rd(self: @This()) u5 {
            return @as(u5, self.prd) + 8;
        }
    };

    pub const CJ = packed struct { op: u2, target: i11, funct3: u3 };

    pub const C = packed struct { op: u2, _: u11, funct3: u3 };

    opcode: u2,
    cr: CR,
    ci: CI,
    css: CSS,
    ciw: CIW,
    cl: CL,
    cs: CS,
    ca: CA,
    cb: CB,
    cj: CJ,
    c: C,

    const Self = @This();

    pub fn from_u16(value: u16) Self {
        return @bitCast(value);
    }

    pub fn to_u16(self: Self) u16 {
        return @bitCast(self);
    }

    pub fn to_varinstr(self: Self) VarInstr {
        return .{ .x16 = self.to_u16() };
    }
};
