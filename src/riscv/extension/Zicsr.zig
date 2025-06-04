const std = @import("std");
const base = @import("../base.zig");

const CSRAddr = base.CSRAddr;

const rearrange = base.rearrange;

const debug = std.debug;

const Arch = base.Arch;
const IFX32 = base.InstrFormatX32;

const Instruction = base.Instruction;

pub const HartMode = enum(u2) {
    U = 0,
    S = 1,
    H = 2,
    M = 3,

    pub fn to_u2(self: @This()) u2 {
        return @intFromEnum(self);
    }

    pub fn from_u2(value: u2) @This() {
        return @enumFromInt(value);
    }

    pub fn name(self: @This()) []const u8 {
        return switch (self) {
            .U => "U",
            .S => "S",
            .H => "H",
            .M => "M",
        };
    }
};

pub const Permisions = enum(u2) {
    None = 0,
    Read = 1,
    Write = 2,
    ReadWrite = 3,

    pub fn read(self: @This()) bool {
        return (self.to_u2() & 1) > 0;
    }

    pub fn write(self: @This()) bool {
        return (self.to_u2() & 2) > 0;
    }

    pub fn to_u2(self: @This()) u2 {
        return @intFromEnum(self);
    }

    pub fn from_u2(value: u2) @This() {
        return @enumFromInt(value);
    }
};

fn has_csr_permisions(mode: HartMode, addr: u12) Permisions {
    switch (mode) {
        .U => {
            if (addr >= 0x000 and 0x0FF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0x400 and 0x4FF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0x800 and 0x8FF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0xC00 and 0xCBF > addr) {
                return .Read;
            }
            if (addr >= 0xCC0 and 0xCFF > addr) {
                return .Read;
            }
        },
        .S => {
            if (addr >= 0x100 and 0x1FF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0x500 and 0x57F > addr) {
                return .ReadWrite;
            }
            if (addr >= 0x580 and 0x5BF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0x900 and 0x97F > addr) {
                return .ReadWrite;
            }
            if (addr >= 0x980 and 0x9BF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0x9C0 and 0x9FF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0xD00 and 0xD7F > addr) {
                return .Read;
            }
            if (addr >= 0xD80 and 0xD8F > addr) {
                return .Read;
            }
            if (addr >= 0xDC0 and 0xDFF > addr) {
                return .Read;
            }
            return has_csr_permisions(.U, addr);
        },
        .H => {
            if (addr >= 0x200 and 0x2FF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0x600 and 0x67F > addr) {
                return .ReadWrite;
            }
            if (addr >= 0x680 and 0x6BF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0x680 and 0x6BF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0x6C0 and 0x6FF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0xA00 and 0xA7F > addr) {
                return .ReadWrite;
            }
            if (addr >= 0xA80 and 0xABF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0xAC0 and 0xAFF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0xE00 and 0xE7F > addr) {
                return .ReadWrite;
            }
            if (addr >= 0xE80 and 0xEBF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0xEC0 and 0xEFF > addr) {
                return .ReadWrite;
            }
            return has_csr_permisions(.S, addr);
        },
        .M => {
            if (addr >= 0x300 and 0x3FF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0x700 and 0x77F > addr) {
                return .ReadWrite;
            }
            if (addr >= 0x780 and 0x79F > addr) {
                return .ReadWrite;
            }
            if (addr >= 0x7A0 and 0x7AF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0x7B0 and 0x7BF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0x7C0 and 0x7FF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0xB00 and 0xB7F > addr) {
                return .ReadWrite;
            }
            if (addr >= 0xB80 and 0xBBF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0xBC0 and 0xBFF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0xF00 and 0xF7F > addr) {
                return .ReadWrite;
            }
            if (addr >= 0xF80 and 0xFBF > addr) {
                return .ReadWrite;
            }
            if (addr >= 0xFC0 and 0xFFF > addr) {
                return .ReadWrite;
            }
            return has_csr_permisions(.H, addr);
        },
    }

    return .None;
}

pub const PMPCFG = packed struct { R: u1, W: u1, X: u1, A: u2, _zero: u2, L: u1 };

pub const X64SSTATUS = packed struct { WPRI0: u1, SIE: u1, WPRI1: u3, SPIE: u1, UBE: u1, WPRI2: u1, SPP: u1, VS: u2, WPRI3: u2, FS: u2, XS: u2, WPRI4: u1, SUM: u1, MXR: u1, WPRI5: u3, SPELP: u1, SDT: u1, WPRI6: u7, UXL: u2, WPRI7: u29, SD: u1 };
pub const X64SATP = packed struct { PPN: u44, ASID: u16, MODE: u4 };
pub const X64TVEC = packed struct { mode: u2, base: u62 };
pub const X64MSTATUS = packed struct { WPRI0: u1, SIE: u1, WPRI1: u1, MIE: u1, WPRI2: u1, SPIE: u1, UBE: u1, MPIE: u1, SPP: u1, VS: u2, MPP: u2, FS: u2, XS: u2, MPRV: u1, SUM: u1, MXR: u1, TVM: u1, TW: u1, TSR: u1, SPELP: u1, SDT: u1, WPRI3: u7, UXL: u2, SXL: u2, SBE: u1, MBE: u1, GVA: u1, MPV: u1, WPRI4: u1, MPELP: u1, MDT: u1, WPRI5: u20, SD: u1 };
pub const X64MIP = packed struct { _zero0: u1, SSIP: u1, _zero1: u1, MSIP: u1, _zero2: u1, STIP: u1, _zero3: u1, MTIP: u1, _zero4: u1, SEIP: u1, _zero5: u1, MEIP: u1, _zero6: u1, LCOFIP: u1, _zero7: u2, platform: u48 };
pub const X64MIE = packed struct { _zero0: u1, SSIE: u1, _zero1: u1, MSIE: u1, _zero2: u1, STIE: u1, _zero3: u1, MTIE: u1, _zero4: u1, SEIE: u1, _zero5: u1, MEIE: u1, _zero6: u1, LCOFIE: u1, _zero7: u2, platform: u48 };
pub const X64PMPCFG_N = packed struct {
    pmpcfg0: PMPCFG,
    pmpcfg1: PMPCFG,
    pmpcfg2: PMPCFG,
    pmpcfg3: PMPCFG,
    pmpcfg4: PMPCFG,
    pmpcfg5: PMPCFG,
    pmpcfg6: PMPCFG,
    pmpcfg7: PMPCFG,
};

fn buildCause(ARCH: Arch) type {
    return packed struct {
        code: @Type(std.builtin.Type{ .int = .{ .signedness = .unsigned, .bits = ARCH.bytes() - 1 } }),
        interrupt: u1,

        pub const InstructionAddrMisaligned = @This(){ .interrupt = 0, .code = 0 };
        pub const InstructionAccesFalut = @This(){ .interrupt = 0, .code = 1 };
        pub const IllegalInstruction = @This(){ .interrupt = 0, .code = 2 };
        pub const Breakpoint = @This(){ .interrupt = 0, .code = 3 };
        pub const LoadAddressMisaligned = @This(){ .interrupt = 0, .code = 4 };
        pub const LoadAccessFault = @This(){ .interrupt = 0, .code = 5 };
        pub const Store_AMOAddressMisaligned = @This(){ .interrupt = 0, .code = 6 };
        pub const Store_AMOAccessFault = @This(){ .interrupt = 0, .code = 7 };
        pub const ECallFromU = @This(){ .interrupt = 0, .code = 8 };
        pub const ECallFromS = @This(){ .interrupt = 0, .code = 9 };

        pub const ECallFromM = @This(){ .interrupt = 0, .code = 11 };
        pub const InstructionPageFault = @This(){ .interrupt = 0, .code = 12 };
        pub const LoadPageFault = @This(){ .interrupt = 0, .code = 13 };

        pub const Store_AMOPageFault = @This(){ .interrupt = 0, .code = 15 };
    };
}

fn buildMNStatus(ARCH: Arch) type {
    return packed struct { _reserved0: u3, NMIE: u1, _reserved1: u3, MNPV: u1, _reserved2: u1, MNPELP: u1, _reserved3: u1, MNPP: u2, _reserved4: @Type(std.builtin.Type{ .int = .{ .signedness = .unsigned, .bits = ARCH.bytes() - 13 } }) };
}

pub const MCOUNTEREN = packed struct { CY: u1, TM: u1, IR: u1, HMP3: u1, HMP4: u1, HMP5: u1, HMP6: u1, HMP7: u1, HMP8: u1, HMP9: u1, HMP10: u1, HMP11: u1, HMP12: u1, HMP13: u1, HMP14: u1, HMP15: u1, HMP16: u1, HMP17: u1, HMP18: u1, HMP19: u1, HMP20: u1, HMP21: u1, HMP22: u1, HMP23: u1, HMP24: u1, HMP25: u1, HMP26: u1, HMP27: u1, HMP28: u1, HMP29: u1, HMP30: u1, HMP31: u1 };
pub const MENVCFG = packed struct { FIOM: u1, WPRI0: u1, LPE: u1, SSE: u1, CBIE: u2, CBCFE: u1, CBZE: u1, WPRI1: u8, WPRI2: u16, PMM: u2, WPRI3: u14, WPRI4: u11, DTE: u1, CDE: u1, ADUE: u1, PBMTE: u1, STCE: u1 };

pub const X32SSTATUS = packed struct { WPRI0: u1, SIE: u1, WPRI1: u3, SPIE: u1, UBE: u1, WPRI2: u1, SPP: u1, VS: u2, WPRI3: u2, FS: u2, XS: u2, WPRI4: u1, SUM: u1, MXR: u1, WPRI5: u3, SPELP: u1, SDT: u1, WPRI6: u6, SD: u1 };

pub const X32SATP = packed struct { PPN: u22, ASID: u9, MODE: u1 };

pub const X32MSTATUS = packed struct { WPRI0: u1, SIE: u1, WPRI1: u1, MIE: u1, WPRI2: u1, SPIE: u1, UBE: u1, MPIE: u1, SPP: u1, VS: u2, MPP: u2, FS: u2, XS: u2, MPRV: u1, SUM: u1, MXR: u1, TVM: u1, TW: u1, TSR: u1, SPELP: u1, SDT: u1, WPRI3: u6, SD: u1 };
pub const X32MSTATUSH = packed struct { WPRI0: u4, SBE: u1, MBE: u1, GVA: u1, MPV: u1, WPRI1: u1, MPELP: u1, MDT: u1, WPRI2: u21 };
pub const X32TVEC = packed struct { mode: u2, base: u30 };
pub const X32MIP = packed struct { _zero0: u1, SSIP: u1, _zero1: u1, MSIP: u1, _zero2: u1, STIP: u1, _zero3: u1, MTIP: u1, _zero4: u1, SEIP: u1, _zero5: u1, MEIP: u1, _zero6: u1, LCOFIP: u1, _zero7: u2, platform: u16 };
pub const X32MIE = packed struct { _zero0: u1, SSIE: u1, _zero1: u1, MSIE: u1, _zero2: u1, STIE: u1, _zero3: u1, MTIE: u1, _zero4: u1, SEIE: u1, _zero5: u1, MEIE: u1, _zero6: u1, LCOFIE: u1, _zero7: u2, platform: u16 };
pub const X32PMPCFG_N = packed struct {
    pmpcfg0: PMPCFG,
    pmpcfg1: PMPCFG,
    pmpcfg2: PMPCFG,
    pmpcfg3: PMPCFG,
};

pub fn buildDataHart(comptime ARCH: Arch) type {
    const uarch = ARCH.uarch();

    return struct {
        pub const CAUSE = buildCause(ARCH);
        pub const MNSTATUS = buildMNStatus(ARCH);

        mode: HartMode,
        xlen: Arch = ARCH,

        time: u64,

        /// # Supervisor Trap Setup
        sstatus: u64,
        sie: uarch,
        stvec: uarch,
        scounteren: u32,

        /// # Supervisor Trap Handling
        sscratch: uarch,
        sepc: uarch,
        scause: uarch,
        stval: uarch,
        sip: uarch,
        // scountovf

        /// # Supervisor Protection and Translation
        satp: uarch,

        stimecmp: u64,

        /// # Machine Information Registers
        mvendorid: uarch,
        marchid: uarch,
        mimpid: uarch,
        mhartid: uarch,
        mconfigptr: uarch,

        /// # Machine Trap Setup
        mstatus: u64,
        misa: uarch,
        medeleg: uarch,
        mideleg: uarch,
        mie: uarch,
        mtvec: uarch,
        mcounteren: MCOUNTEREN,

        /// # Machine Trap Handling
        mscratch: uarch,
        mepc: uarch,
        mcause: CAUSE,
        mtval: uarch,
        mip: uarch,
        mtinst: uarch,
        mtval2: uarch,

        /// # Machine Configuration
        menvcfg: MENVCFG,
        // mseccfg

        /// # Machine Memory Protection
        pmpcfg0_1: u64,
        pmpcfg2_3: u64,
        // .. pmpcfg15
        pmpaddr0: uarch,
        // .. pmpaddr63

        /// # Machine Non-Maskable Interrupt Handling
        mnscratch: uarch,
        mnepc: uarch,
        mncause: CAUSE,
        mnstatus: MNSTATUS,

        fn csr_store(self: *@This(), csr_addr: u12, value: uarch) !void {
            switch (csr_addr) {
                CSRAddr.sstatus.to_u12() => self.sstatus = value,
                CSRAddr.sie.to_u12() => self.sie = value,
                CSRAddr.stvec.to_u12() => self.stvec = value,
                CSRAddr.scounteren.to_u12() => self.scounteren = @truncate(value),

                CSRAddr.sscratch.to_u12() => self.sscratch = value,
                CSRAddr.sepc.to_u12() => self.sepc = value,
                CSRAddr.scause.to_u12() => self.scause = value,
                CSRAddr.stval.to_u12() => self.stval = value,
                CSRAddr.sip.to_u12() => self.sip = value,

                CSRAddr.satp.to_u12() => switch (self.xlen) {
                    .X32 => {
                        self.satp = @truncate(value);
                        const mstatus = @as(X32MSTATUS, @bitCast(@as(u32, @truncate(self.mstatus))));
                        const satp = @as(X32SATP, @bitCast(@as(u32, @truncate(self.satp))));
                        std.debug.print("SATP: {}\n", .{satp});

                        if (mstatus.TVM == 1) {
                            return error.TVM_IS_ON;
                        }

                        switch (satp.MODE) {
                            0 => {}, // Bare
                            1 => { // Sv32
                                const root = @as(uarch, satp.PPN) * std.math.pow(uarch, 2, 10) * 4;
                                std.debug.print("Root: 0x{x}\n", .{root});
                            },
                        }
                    },
                    .X64 => {
                        if (ARCH == .X32) unreachable;
                        self.satp = @truncate(value);
                        const mstatus = @as(X64MSTATUS, @bitCast(@as(u64, @truncate(self.mstatus))));
                        const satp = @as(X64SATP, @bitCast(@as(u64, @truncate(self.satp))));
                        std.debug.print("SATP: {}\n", .{satp});

                        if (mstatus.TVM == 1) {
                            return error.TVM_IS_ON;
                        }

                        switch (satp.MODE) {
                            0 => {}, // Bare
                            else => {
                                @panic("Not implemented");
                            },
                        }
                    },
                },

                CSRAddr.stimecmp.to_u12() => switch (self.xlen) {
                    .X32 => {
                        self.stimecmp = (self.stimecmp & (0xffffffff << 32)) | value;
                    },
                    .X64 => self.stimecmp = value,
                },
                CSRAddr.stimecmph.to_u12() => if (self.xlen == .X32) {
                    self.stimecmp = (self.stimecmp & 0xffffffff) | (@as(u64, value) << 32);
                },

                CSRAddr.mstatus.to_u12() => self.mstatus = value,
                CSRAddr.misa.to_u12() => self.misa = value,
                CSRAddr.mideleg.to_u12() => self.mideleg = value,
                CSRAddr.medeleg.to_u12() => self.medeleg = value,
                CSRAddr.mie.to_u12() => self.mie = value,
                CSRAddr.mtvec.to_u12() => {
                    // TODO implement Vectored
                    switch (self.xlen) {
                        .X32 => {
                            self.mtvec = @as(u32, @truncate(value));
                            const mtvec = @as(*X32TVEC, @ptrCast(&self.mtvec));
                            mtvec.mode = 0;
                        },
                        .X64 => {
                            if (ARCH == .X32) unreachable;
                            self.mtvec = @as(u64, @truncate(value));
                            const mtvec = @as(*X64TVEC, @ptrCast(&self.mtvec));
                            mtvec.mode = 0;
                        },
                    }
                },
                CSRAddr.mcounteren.to_u12() => self.mcounteren = @bitCast(@as(u32, @truncate(value))),

                CSRAddr.mscratch.to_u12() => self.mscratch = value,
                CSRAddr.mepc.to_u12() => self.mepc = value,
                CSRAddr.mcause.to_u12() => self.mcause = @bitCast(value),
                CSRAddr.mtval.to_u12() => self.mtval = value,
                CSRAddr.mip.to_u12() => self.mip = value,
                CSRAddr.mtinst.to_u12() => self.mtinst = value,
                CSRAddr.mtval2.to_u12() => self.mtval2 = value,

                CSRAddr.menvcfg.to_u12() => switch (self.xlen) {
                    .X32 => {
                        const mask: u64 = 0xffffffff;
                        const menvcfg: u64 = @bitCast(self.menvcfg);
                        self.menvcfg = @bitCast(((menvcfg & mask) ^ (menvcfg & mask)) | @as(u32, @truncate(value)));
                    },
                    .X64 => {
                        if (ARCH == .X32) unreachable;
                        self.menvcfg = @bitCast(@as(u64, @truncate(value)));
                    },
                },
                CSRAddr.menvcfgh.to_u12() => if (self.xlen == .X32) {
                    const mask: u64 = 0xffffffff << 32;
                    const menvcfg: u64 = @bitCast(self.menvcfg);
                    self.menvcfg = @bitCast(((menvcfg & mask) ^ (menvcfg & mask)) | (@as(u64, value) << 32));
                },

                CSRAddr.pmpcfg0.to_u12() => self.pmpcfg0_1 = value,
                CSRAddr.pmpaddr0.to_u12() => self.pmpaddr0 = value,

                // CSRAddr.mnscratch.to_u12() => self.mnscratch = value,
                // CSRAddr.mnepc.to_u12() => self.mnepc = value,
                // CSRAddr.mncause.to_u12() => self.mncause = @bitCast(value),
                CSRAddr.mnstatus.to_u12() => self.mnstatus = @bitCast(value),

                else => {
                    std.debug.print("CSR_STORE: Unknown CSR: 0x{x}\n", .{csr_addr});
                    return error.UnknownCSR;
                },
            }
        }

        fn csr_load(self: *@This(), csr_addr: u12) !uarch {
            switch (csr_addr) {
                CSRAddr.time.to_u12() => return @truncate(self.time),

                CSRAddr.sstatus.to_u12() => return @truncate(self.sstatus),
                CSRAddr.sie.to_u12() => return self.sie,
                CSRAddr.stvec.to_u12() => return self.stvec,
                CSRAddr.scounteren.to_u12() => return self.scounteren,

                CSRAddr.sscratch.to_u12() => return self.sscratch,
                CSRAddr.sepc.to_u12() => return self.sepc,
                CSRAddr.scause.to_u12() => return self.scause,
                CSRAddr.stval.to_u12() => return self.stval,
                CSRAddr.sip.to_u12() => return self.sip,

                CSRAddr.satp.to_u12() => {
                    switch (self.xlen) {
                        .X32 => {
                            const mstatus = @as(X32MSTATUS, @bitCast(@as(u32, @truncate(self.mstatus))));
                            if (mstatus.TVM == 1) {
                                return error.TVM_IS_ON;
                            }
                            return @as(u32, @truncate(self.satp));
                        },
                        .X64 => {
                            if (ARCH == .X32) unreachable;
                            const mstatus = @as(X32MSTATUS, @bitCast(@as(u32, @truncate(self.mstatus))));
                            if (mstatus.TVM == 1) {
                                return error.TVM_IS_ON;
                            }
                            return @as(u64, @truncate(self.satp));
                        },
                    }
                },

                CSRAddr.stimecmp.to_u12() => return @truncate(self.stimecmp),

                CSRAddr.mvendorid.to_u12() => return self.mvendorid,
                CSRAddr.marchid.to_u12() => return self.marchid,
                CSRAddr.mimpid.to_u12() => return self.mimpid,
                CSRAddr.mhartid.to_u12() => return self.mhartid,
                CSRAddr.mconfigptr.to_u12() => return self.mconfigptr,

                CSRAddr.mstatus.to_u12() => return @truncate(self.mstatus),
                CSRAddr.misa.to_u12() => return self.misa,
                CSRAddr.mideleg.to_u12() => return self.mideleg,
                CSRAddr.medeleg.to_u12() => return self.medeleg,
                CSRAddr.mie.to_u12() => return self.mie,
                CSRAddr.mtvec.to_u12() => return self.mtvec,

                CSRAddr.mscratch.to_u12() => return self.mscratch,
                CSRAddr.mepc.to_u12() => return self.mepc,
                CSRAddr.mcause.to_u12() => return @bitCast(self.mcause),
                CSRAddr.mtval.to_u12() => return self.mtval,
                CSRAddr.mcounteren.to_u12() => return @as(u32, @bitCast(self.mcounteren)),
                CSRAddr.mip.to_u12() => return self.mip,
                CSRAddr.mtinst.to_u12() => return self.mtinst,
                CSRAddr.mtval2.to_u12() => return self.mtval2,

                CSRAddr.menvcfg.to_u12() => return @truncate(@as(u64, @bitCast(self.menvcfg))),
                CSRAddr.menvcfgh.to_u12() => if (self.xlen == .X32) {
                    return @truncate(@as(u64, @bitCast(self.menvcfg)) >> 32);
                } else {
                    return 0;
                },

                CSRAddr.pmpcfg0.to_u12() => return @truncate(self.pmpcfg0_1),
                CSRAddr.pmpaddr0.to_u12() => return self.pmpaddr0,
                else => {
                    std.debug.print("CSR_LOAD: Unknown CSR\n", .{});
                    return error.UnknownCSR;
                },

                // CSRAddr.mnscratch.to_u12() => return self.mnscratch,
                // CSRAddr.mnepc.to_u12() => return self.mnepc,
                // CSRAddr.mncause.to_u12() => return @bitCast(self.mncause),
                // CSRAddr.mnstatus.to_u12() => return @bitCast(self.mnstatus),
            }
        }
    };
}

pub fn CSRRW(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        pub fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;
            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1110011) return false; // SYSTEM opcode
            if (x32_instr.i.funct3 != 0b001) return false; // CSRRW func3

            return true;
        }

        pub fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            std.debug.assert(instr_data.len == 4);

            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;

            const csr_addr: u12 = @bitCast(i.imm_11_0);
            const per = has_csr_permisions(hart_data.Zicsr.mode, csr_addr);
            if (!(per.read() and per.write())) {
                hart_data.illegal_instruction();
                return;
            }

            if (i.rd != 0) {
                hart_data.I.regs[i.rd] = hart_data.Zicsr.csr_load(csr_addr) catch {
                    hart_data.illegal_instruction();
                    return;
                };
            }

            const value = hart_data.I.regs[i.rs1];

            hart_data.Zicsr.csr_store(csr_addr, value) catch {
                hart_data.illegal_instruction();
                return;
            };

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

pub fn CSRRS(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        pub fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;
            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1110011) return false; // SYSTEM opcode
            if (x32_instr.i.funct3 != 0b010) return false; // CSRRS func3

            return true;
        }

        pub fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            std.debug.assert(instr_data.len == 4);

            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;

            const csr_addr: u12 = @bitCast(i.imm_11_0);
            const per = has_csr_permisions(hart_data.Zicsr.mode, csr_addr);
            if (!per.read()) {
                hart_data.illegal_instruction();
                return;
            }

            const tmp =
                hart_data.Zicsr.csr_load(csr_addr) catch {
                    hart_data.illegal_instruction();
                    return;
                };

            if (i.rd != 0) {
                hart_data.I.regs[i.rd] = tmp;
            }

            const value = hart_data.I.regs[i.rs1];

            if (value != 0) {
                if (!per.write()) {
                    hart_data.illegal_instruction();
                    return;
                }
                hart_data.Zicsr.csr_store(csr_addr, tmp | value) catch {
                    hart_data.illegal_instruction();
                    return;
                };
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

pub fn CSRRC(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        pub fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;
            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1110011) return false; // SYSTEM opcode
            if (x32_instr.i.funct3 != 0b011) return false; // CSRRC func3

            return true;
        }

        pub fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            std.debug.assert(instr_data.len == 4);

            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;

            const csr_addr: u12 = @bitCast(i.imm_11_0);
            const per = has_csr_permisions(hart_data.Zicsr.mode, csr_addr);
            if (!(per.read() and per.write())) {
                hart_data.illegal_instruction();
                return;
            }

            const tmp =
                hart_data.Zicsr.csr_load(csr_addr) catch {
                    hart_data.illegal_instruction();
                    return;
                };

            if (i.rd != 0) {
                hart_data.I.regs[i.rd] = tmp;
            }

            const value = hart_data.I.regs[i.rs1];

            hart_data.Zicsr.csr_store(csr_addr, tmp ^ (value & tmp)) catch {
                hart_data.illegal_instruction();
                return;
            };

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

pub fn CSRRWI(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        pub fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;
            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1110011) return false; // SYSTEM opcode
            if (x32_instr.i.funct3 != 0b101) return false; // CSRRWI func3

            return true;
        }

        pub fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            std.debug.assert(instr_data.len == 4);

            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;

            const csr_addr: u12 = @bitCast(i.imm_11_0);
            const per = has_csr_permisions(hart_data.Zicsr.mode, csr_addr);
            if (!(per.read() and per.write())) {
                hart_data.illegal_instruction();
                return;
            }

            if (i.rd != 0) {
                hart_data.I.regs[i.rd] = hart_data.Zicsr.csr_load(csr_addr) catch {
                    hart_data.illegal_instruction();
                    return;
                };
            }

            const value = i.rs1;

            hart_data.Zicsr.csr_store(csr_addr, value) catch {
                hart_data.illegal_instruction();
                return;
            };

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

pub fn CSRRSI(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        pub fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;
            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1110011) return false; // SYSTEM opcode
            if (x32_instr.i.funct3 != 0b110) return false; // CSRRSI func3

            return true;
        }

        pub fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            std.debug.assert(instr_data.len == 4);

            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;

            const csr_addr: u12 = @bitCast(i.imm_11_0);
            const per = has_csr_permisions(hart_data.Zicsr.mode, csr_addr);
            if (!per.read()) {
                hart_data.illegal_instruction();
                return;
            }

            const tmp =
                hart_data.Zicsr.csr_load(csr_addr) catch {
                    hart_data.illegal_instruction();
                    return;
                };

            if (i.rd != 0) {
                hart_data.I.regs[i.rd] = tmp;
            }

            const value = i.rs1;

            if (value != 0) {
                if (!per.write()) {
                    hart_data.illegal_instruction();
                    return;
                }
                hart_data.Zicsr.csr_store(csr_addr, tmp | value) catch {
                    hart_data.illegal_instruction();
                    return;
                };
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

pub fn CSRRCI(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        pub fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;
            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1110011) return false; // SYSTEM opcode
            if (x32_instr.i.funct3 != 0b111) return false; // CSRRCI func3

            return true;
        }

        pub fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            std.debug.assert(instr_data.len == 4);

            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));
            const i = x32_instr.i;

            const csr_addr: u12 = @bitCast(i.imm_11_0);
            const per = has_csr_permisions(hart_data.Zicsr.mode, csr_addr);
            if (!(per.read() and per.write())) {
                hart_data.illegal_instruction();
                return;
            }

            const tmp =
                hart_data.Zicsr.csr_load(csr_addr) catch {
                    hart_data.illegal_instruction();
                    return;
                };

            if (i.rd != 0) {
                hart_data.I.regs[i.rd] = tmp;
            }

            const value = i.rs1;

            hart_data.Zicsr.csr_store(csr_addr, tmp ^ (value & tmp)) catch {
                hart_data.illegal_instruction();
                return;
            };

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

pub fn SRET(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        pub fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;
            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1110011) return false; // SYSTEM opcode
            if (x32_instr.i.rd != 0b000) return false; // SRET rd
            if (x32_instr.i.funct3 != 0b000) return false; // SRET func3
            if (x32_instr.i.rs1 != 0b000) return false; // SRET rs1
            if (x32_instr.i.imm_11_0 != 0b000100000010) return false; // SRET

            return true;
        }

        pub fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            std.debug.assert(instr_data.len == 4);

            if (hart_data.Zicsr.mode != .S and hart_data.Zicsr.mode != .M) {
                std.debug.print("SRET: HartMode is not M or S, is {s}\n", .{hart_data.Zicsr.mode.name()});
                hart_data.illegal_instruction();
                return;
            }

            switch (hart_data.Zicsr.xlen) {
                .X32 => {
                    if (@as(X32MSTATUS, @bitCast(@as(u32, @truncate(hart_data.Zicsr.mstatus)))).TSR == 1) {
                        hart_data.illegal_instruction();
                        return;
                    }

                    hart_data.I.pc = hart_data.Zicsr.sepc;
                    const sstatus = @as(*X32SSTATUS, @ptrCast(&hart_data.Zicsr.sstatus));
                    sstatus.SIE = sstatus.SPIE;
                    if (sstatus.SPP == 0) {
                        sstatus.SDT = 0;
                    }
                    hart_data.Zicsr.mode = HartMode.from_u2(sstatus.SPP);
                    sstatus.SPIE = 1;
                    sstatus.SPP = @truncate(HartMode.U.to_u2());
                },
                .X64 => {
                    if (ARCH == .X32) unreachable;
                    if (@as(X64MSTATUS, @bitCast(@as(u64, @truncate(hart_data.Zicsr.mstatus)))).TSR == 1) {
                        hart_data.illegal_instruction();
                        return;
                    }

                    hart_data.I.pc = hart_data.Zicsr.sepc;
                    const sstatus = @as(*X64SSTATUS, @ptrCast(&hart_data.Zicsr.sstatus));
                    sstatus.SIE = sstatus.SPIE;
                    if (sstatus.SPP == 0) {
                        sstatus.SDT = 0;
                    }
                    hart_data.Zicsr.mode = HartMode.from_u2(sstatus.SPP);

                    if (sstatus.SPP == HartMode.U.to_u2()) {
                        switch (sstatus.UXL) {
                            1 => hart_data.Zicsr.xlen = .X32,
                            2 => hart_data.Zicsr.xlen = .X64,
                            else => {},
                        }
                    }

                    sstatus.SPIE = 1;
                    sstatus.SPP = @truncate(HartMode.U.to_u2());
                },
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

pub fn MRET(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        pub fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;
            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1110011) return false; // SYSTEM opcode
            if (x32_instr.i.rd != 0b000) return false; // MRET rd
            if (x32_instr.i.funct3 != 0b000) return false; // MRET func3
            if (x32_instr.i.rs1 != 0b000) return false; // MRET rs1
            if (x32_instr.i.imm_11_0 != 0b001100000010) return false; // MRET

            return true;
        }

        pub fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            std.debug.assert(instr_data.len == 4);

            if (hart_data.Zicsr.mode != .M) {
                std.debug.print("MRET: HartMode is not M, is {s}\n", .{hart_data.Zicsr.mode.name()});
                hart_data.illegal_instruction();
                return;
            }

            switch (hart_data.Zicsr.xlen) {
                .X32 => {
                    hart_data.I.pc = @as(u32, @truncate(hart_data.Zicsr.mepc));
                    const mstatus = @as(*X32MSTATUS, @ptrCast(&hart_data.Zicsr.mstatus));
                    const mstatush = @as(*X32MSTATUSH, @ptrCast(@as(*[2]u32, @ptrCast(&hart_data.Zicsr.mstatus))[1..2]));
                    mstatus.MIE = mstatus.MPIE;
                    mstatush.MDT = 0;
                    hart_data.Zicsr.mode = HartMode.from_u2(mstatus.MPP);
                    mstatus.MPIE = 1;
                    mstatus.MPP = HartMode.U.to_u2();
                },
                .X64 => {
                    if (ARCH == .X32) unreachable;
                    hart_data.I.pc = @as(u64, @truncate(hart_data.Zicsr.mepc));
                    const mstatus = @as(*X64MSTATUS, @ptrCast(&hart_data.Zicsr.mstatus));
                    mstatus.MIE = mstatus.MPIE;
                    mstatus.MDT = 0;
                    hart_data.Zicsr.mode = HartMode.from_u2(mstatus.MPP);

                    if (mstatus.MPP == HartMode.U.to_u2()) {
                        switch (mstatus.UXL) {
                            1 => hart_data.Zicsr.xlen = .X32,
                            2 => hart_data.Zicsr.xlen = .X64,
                            else => {},
                        }
                    }

                    if (mstatus.MPP == HartMode.S.to_u2()) {
                        switch (mstatus.SXL) {
                            1 => hart_data.Zicsr.xlen = .X32,
                            2 => hart_data.Zicsr.xlen = .X64,
                            else => {},
                        }
                    }

                    mstatus.MPIE = 1;
                    mstatus.MPP = HartMode.U.to_u2();
                },
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

pub fn MNRET(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        pub fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;
            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1110011) return false; // SYSTEM opcode
            if (x32_instr.i.rd != 0b000) return false; // MRET rd
            if (x32_instr.i.funct3 != 0b000) return false; // MRET func3
            if (x32_instr.i.rs1 != 0b000) return false; // MRET rs1
            if (x32_instr.i.imm_11_0 != 0b011100000010) return false; // MRET

            return true;
        }

        pub fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            std.debug.assert(instr_data.len == 4);

            if (hart_data.Zicsr.mode != .M) {
                std.debug.print("MNRET: HartMode is not M, is {s}\n", .{hart_data.Zicsr.mode.name()});
                hart_data.illegal_instruction();
                return;
            }
            hart_data.I.pc = hart_data.Zicsr.mnepc;
            const mnstatus = &hart_data.Zicsr.mnstatus;
            hart_data.Zicsr.mode = HartMode.from_u2(mnstatus.MNPP);
            if (hart_data.Zicsr.mode != .M) {
                mnstatus.NMIE = 1;
            }
            mnstatus.MNPP = HartMode.U.to_u2();
        }

        pub fn instr() Instruction(ARCH, DataEEI, DataHart) {
            return .{
                .check = &@This().check,
                .execute = &@This().execute,
            };
        }
    };
}

pub fn WFI(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        pub fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;
            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1110011) return false; // SYSTEM opcode
            if (x32_instr.i.rd != 0b000) return false; // WFI rd
            if (x32_instr.i.funct3 != 0b000) return false; // WFI func3
            if (x32_instr.i.rs1 != 0b000) return false; // WFI rs1
            if (x32_instr.i.imm_11_0 != 0b000100000101) return false; // WFI

            return true;
        }

        pub fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            std.debug.assert(instr_data.len == 4);

            switch (hart_data.Zicsr.xlen) {
                .X32 => {
                    if (@as(X32MSTATUS, @bitCast(@as(u32, @truncate(hart_data.Zicsr.mstatus)))).TW == 1) {
                        std.debug.print("Timeout Wait\n", .{});
                        hart_data.illegal_instruction();
                        return;
                    }
                },
                .X64 => {
                    if (ARCH == .X32) unreachable;
                    if (@as(X64MSTATUS, @bitCast(@as(u64, @truncate(hart_data.Zicsr.mstatus)))).TW == 1) {
                        std.debug.print("Timeout Wait\n", .{});
                        hart_data.illegal_instruction();
                        return;
                    }
                },
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

pub fn SFENCE_VMA(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) type {
    return struct {
        pub fn check(instr_data: []const u8) bool {
            if (instr_data.len != 4) return false;
            const x32_instr = IFX32.from_u32(std.mem.readInt(u32, @ptrCast(instr_data), .little));

            if (x32_instr.opcode != 0b1110011) return false; // SYSTEM opcode
            if (x32_instr.i.rd != 0b000) return false; // SFENCE_VMA rd
            if (x32_instr.i.funct3 != 0b000) return false; // SFENCE_VMA func3
            if (x32_instr.i.imm_11_0 >> 5 != 0b1001) return false; // SFENCE_VMA

            return true;
        }

        pub fn execute(eei_data: *DataEEI, hart_data: *DataHart, instr_data: []const u8) void {
            _ = eei_data;

            std.debug.assert(instr_data.len == 4);

            // TODO: SFENCE_VMA
            std.debug.print("SFENCE_VMA not implemented\n", .{});

            switch (hart_data.Zicsr.xlen) {
                .X32 => {
                    if (@as(X32MSTATUS, @bitCast(@as(u32, @truncate(hart_data.Zicsr.mstatus)))).TVM == 1) {
                        hart_data.illegal_instruction();
                        return;
                    }
                },
                .X64 => {
                    if (ARCH == .X32) unreachable;
                    if (@as(X64MSTATUS, @bitCast(@as(u64, @truncate(hart_data.Zicsr.mstatus)))).TVM == 1) {
                        hart_data.illegal_instruction();
                        return;
                    }
                },
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

pub fn buildInstrs(comptime ARCH: Arch, comptime DataEEI: type, comptime DataHart: type) [11]Instruction(ARCH, DataEEI, DataHart) {
    return .{
        CSRRW(ARCH, DataEEI, DataHart).instr(),
        CSRRS(ARCH, DataEEI, DataHart).instr(),
        CSRRC(ARCH, DataEEI, DataHart).instr(),
        CSRRWI(ARCH, DataEEI, DataHart).instr(),
        CSRRSI(ARCH, DataEEI, DataHart).instr(),
        CSRRCI(ARCH, DataEEI, DataHart).instr(),

        SRET(ARCH, DataEEI, DataHart).instr(),
        MRET(ARCH, DataEEI, DataHart).instr(),
        MNRET(ARCH, DataEEI, DataHart).instr(),

        WFI(ARCH, DataEEI, DataHart).instr(),

        SFENCE_VMA(ARCH, DataEEI, DataHart).instr(),
    };
}
