const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("base.zig");
pub const Arch = base.Arch;
pub const IntRegNames = base.IntRegNames;
pub const IntReg = base.IntReg;
pub const VarInstr = base.VarInstr;
pub const InstrFX16 = base.InstrFormatX16;
pub const InstrFX32 = base.InstrFormatX32;
pub const CSRAddr = base.CSRAddr;
pub const CSRAddrU = base.CSRAddrU;
pub const rearrange = base.rearrange;

const print = std.debug.print;

pub const BusEntry = struct {
    // inclusive range
    start: u64,
    end: u64,
    io: *anyopaque,
    fn_read: *const fn (io: *anyopaque, index: u64, buffer: []u8) void,
    fn_write: *const fn (io: *anyopaque, index: u64, buffer: []const u8) void,

    fn read(self: @This(), index: u64, buffer: []u8) void {
        self.fn_read(self.io, index, buffer);
    }

    fn write(self: @This(), index: u64, buffer: []const u8) void {
        self.fn_write(self.io, index, buffer);
    }
};

pub fn buildCPU(comptime arch: Arch, comptime harts_len: usize) type {
    const uarch = arch.uarch();
    const iarch = arch.iarch();
    const bits = arch.bytes();
    const stype: type = switch (arch) {
        .X32 => u5,
        .X64 => u6,
    };

    return struct {
        const CPU = @This();

        pub const UARCH = uarch;

        pub const HartMode = enum(u2) {
            U = 0,
            S = 1,
            H = 2,
            M = 3,

            fn to_u2(self: @This()) u2 {
                return @intFromEnum(self);
            }

            fn from_u2(value: u2) @This() {
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

            fn read(self: @This()) bool {
                return (self.to_u2() & 1) > 0;
            }

            fn write(self: @This()) bool {
                return (self.to_u2() & 2) > 0;
            }

            fn to_u2(self: @This()) u2 {
                return @intFromEnum(self);
            }

            fn from_u2(value: u2) @This() {
                return @enumFromInt(value);
            }
        };

        pub const CSRS = base.buildCSRS(arch);

        pub const Hart = struct {
            g_regs: [32]uarch,
            pc: uarch,
            csrs: CSRS,
            mode: HartMode,

            pub fn step(self: *Hart, cpu: *CPU) !void {
                var memory: [8]u8 = undefined;
                try self.mmio_read(cpu, self.pc, &memory);
                const e_var_instr = VarInstr.from_memory(&memory);

                if (e_var_instr) |var_instr| switch (var_instr) {
                    .x16 => |x16| {
                        const instr = InstrFX16.from_u16(x16);
                        switch (instr.opcode) {
                            0b00 => switch (instr.cl.funct3) {
                                0b000 => self.c_addi4spn(instr),
                                0b010 => try self.c_lw(instr, cpu),
                                0b110 => try self.c_sw(instr, cpu),
                                0b111 => try self.c_sd(instr, cpu),
                                else => {
                                    var_instr.debug();
                                    return error.NotImplemented;
                                },
                            },
                            0b01 => switch (instr.ci.funct3) {
                                0b000 => if (instr.ci.rd != 0) self.c_addi(instr),
                                0b001 => if (instr.ci.rd != 0) self.c_addiw(instr),
                                0b010 => if (instr.ci.rd != 0) self.c_li(instr),
                                0b011 => if (instr.ci.rd != 0) self.c_lui(instr),
                                0b100 => if (instr.ci.rd != 0) switch (@as(u2, @truncate(instr.cb.offset2))) {
                                    0b00 => self.c_srli(instr),
                                    0b10 => self.c_andi(instr),
                                    0b11 => if (instr.ci.imm_12 == 0)
                                        switch (instr.ca.funct2) {
                                            0b10 => self.c_or(instr),
                                            0b11 => self.c_and(instr),
                                            else => {
                                                var_instr.debug();
                                                return error.NotImplemented;
                                            },
                                        }
                                    else {
                                        var_instr.debug();
                                        return error.NotImplemented;
                                    },
                                    else => {
                                        var_instr.debug();
                                        return error.NotImplemented;
                                    },
                                },
                                0b101 => self.c_j(instr),
                                0b110 => self.c_beqz(instr),
                                0b111 => self.c_bnez(instr),
                            },
                            0b10 => switch (instr.ci.funct3) {
                                0b000 => self.c_slli(instr),
                                0b011 => try self.c_ldsp(instr, cpu),
                                0b100 => switch (instr.ci.imm_12) {
                                    0 => try self.c_jr_mv(instr),
                                    1 => self.c_add(instr),
                                },
                                0b111 => try self.c_sdsp(instr, cpu),
                                else => {
                                    var_instr.debug();
                                    return error.NotImplemented;
                                },
                            },
                            else => {
                                var_instr.debug();
                                return error.NotImplemented;
                            },
                        }
                    },
                    .x32 => |x32| {
                        const instr = InstrFX32.from_u32(x32);

                        switch (instr.opcode) {
                            0b0110111 => self.lui(instr),
                            0b0010111 => self.auipc(instr),
                            0b0101111 => try self.amo(instr, cpu),
                            0b1101111 => self.jal(instr),
                            0b1100111 => self.jalr(instr),
                            0b1100011 => self.branch(instr),
                            0b0000011 => try self.load(instr, cpu),
                            0b0100011 => try self.store(instr, cpu),
                            0b0010011 => self.op_imm(instr),
                            0b0011011 => self.op_imm_32(instr),
                            0b0110011 => try self.op(instr),
                            0b0111011 => self.op_32(instr),
                            0b0001111 => self.misc_mem(instr),
                            0b1110011 => try self.system(instr, cpu),
                            else => {
                                instr.debug();
                                return error.NotImplemented;
                            },
                        }
                    },
                    .x64 => |x64| {
                        _ = x64;
                        var_instr.debug();
                        return error.NotImplemented;
                    },
                } else |e| print("When reading VarInstr at {x}, an error acured: {}\n", .{ self.pc, e });
            }

            fn illegal_instruction(self: *@This()) void {
                print("Illegal Instruction\n", .{});
                self.m_trap(CSRS.CAUSE.IllegalInstruction);
            }

            fn trap(self: *@This(), cause: CSRS.CAUSE) bool {
                if (cause.interrupt == 1) @panic("Interrupt not implemented");

                switch (self.mode) {
                    .U, .S => {
                        if (self.csrs.medeleg >> @truncate(cause.code) & 1 == 1) {
                            self.s_trap(cause);
                            return true;
                        } else {
                            self.m_trap(cause);
                            return true;
                        }
                    },
                    .M => {
                        self.m_trap(cause);
                        return true;
                    },
                    .H => {
                        @panic("Not implemented");
                    },
                }
            }

            fn m_trap(self: *@This(), cause: CSRS.CAUSE) void {
                self.csrs.mstatus.MPP = self.mode.to_u2();
                self.mode = .M;
                self.csrs.mepc = self.pc;
                self.csrs.mcause = cause;
                const tvec = self.csrs.mtvec;
                switch (tvec.mode) {
                    0 => { // DIRECT
                        self.pc = @as(uarch, tvec.base) << 2;
                    },
                    1 => { // VECTORED
                        // TODO: Implement VECTORED
                        @panic("VECTORED not implemented!");
                        // self.pc = (mtvec ^ (mtvec & 0b11)) + (4 * (cause & 0xffffffff));
                    },
                    else => {
                        @panic("Unknown MTVEC Mode!");
                    },
                }
            }

            fn s_trap(self: *@This(), cause: CSRS.CAUSE) void {
                self.csrs.sstatus.SPP = @truncate(self.mode.to_u2());
                self.mode = .S;
                self.csrs.sepc = self.pc;
                self.csrs.scause = cause;
                const tvec = self.csrs.stvec;
                switch (tvec.mode) {
                    0 => { // DIRECT
                        self.pc = @as(uarch, tvec.base) << 2;
                    },
                    1 => { // VECTORED
                        // TODO: Implement VECTORED
                        @panic("VECTORED not implemented!");
                        // self.pc = (mtvec ^ (mtvec & 0b11)) + (4 * (cause & 0xffffffff));
                    },
                    else => {
                        @panic("Unknown MTVEC Mode!");
                    },
                }
            }

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

            fn csr_store(self: *@This(), csr_addr: u12, value: uarch) !void {
                switch (csr_addr) {
                    CSRAddr.sstatus.to_u12() => self.csrs.sstatus = @bitCast(value),
                    CSRAddr.sie.to_u12() => self.csrs.sie = value,
                    CSRAddr.stvec.to_u12() => self.csrs.stvec = @bitCast(value),
                    CSRAddr.scounteren.to_u12() => self.csrs.scounteren = @bitCast(@as(u32, @truncate(value))),

                    CSRAddr.sscratch.to_u12() => self.csrs.sscratch = value,
                    CSRAddr.sepc.to_u12() => self.csrs.sepc = value,
                    CSRAddr.scause.to_u12() => self.csrs.scause = @bitCast(value),
                    CSRAddr.stval.to_u12() => self.csrs.stval = value,
                    CSRAddr.sip.to_u12() => self.csrs.sip = @bitCast(value),

                    CSRAddr.satp.to_u12() => {
                        self.csrs.satp = @bitCast(value);
                        print("SATP: {}\n", .{self.csrs.satp});

                        switch (arch) {
                            .X32 => switch (self.csrs.satp.MODE) {
                                0 => {}, // Bare
                                1 => { // Sv32
                                    const root = @as(uarch, self.csrs.satp.PPN) * std.math.pow(uarch, 2, 10) * 4;
                                    print("Root: 0x{x}\n", .{root});
                                },
                            },
                            .X64 => switch (self.csrs.satp.MODE) {
                                0 => {}, // Bare
                                else => {
                                    @panic("Not implemented");
                                },
                            },
                        }
                    },

                    CSRAddr.stimecmp.to_u12() => switch (arch) {
                        .X32 => {
                            self.csrs.stimecmp = ((self.csrs.stimecmp & 0xffffffff) ^ (self.csrs.stimecmp & 0xffffffff)) | value;
                        },
                        .X64 => self.csrs.stimecmp = value,
                    },
                    CSRAddr.stimecmph.to_u12() => if (arch == .X32) {
                        const mask = 0xffffffff << 32;
                        self.csrs.stimecmp = ((self.csrs.stimecmp & mask) ^ (self.csrs.stimecmp & mask)) | (@as(u64, value) << 32);
                    },

                    CSRAddr.mstatus.to_u12() => self.csrs.mstatus = @bitCast(value),
                    CSRAddr.misa.to_u12() => self.csrs.misa = @bitCast(value),
                    CSRAddr.mideleg.to_u12() => self.csrs.mideleg = @bitCast(value),
                    CSRAddr.medeleg.to_u12() => self.csrs.medeleg = @bitCast(value),
                    CSRAddr.mie.to_u12() => self.csrs.mie = @bitCast(value),
                    CSRAddr.mtvec.to_u12() => self.csrs.mtvec = @bitCast(value),
                    CSRAddr.mcounteren.to_u12() => self.csrs.mcounteren = @bitCast(@as(u32, @truncate(value))),

                    CSRAddr.mscratch.to_u12() => self.csrs.mscratch = value,
                    CSRAddr.mepc.to_u12() => self.csrs.mepc = value,
                    CSRAddr.mcause.to_u12() => self.csrs.mcause = @bitCast(value),
                    CSRAddr.mtval.to_u12() => self.csrs.mtval = value,
                    CSRAddr.mip.to_u12() => self.csrs.mip = @bitCast(value),
                    CSRAddr.mtinst.to_u12() => self.csrs.mtinst = value,
                    CSRAddr.mtval2.to_u12() => self.csrs.mtval2 = value,

                    CSRAddr.menvcfg.to_u12() => switch (arch) {
                        .X32 => {
                            const mask: u64 = 0xffffffff;
                            const menvcfg: u64 = @bitCast(self.csrs.menvcfg);
                            self.csrs.menvcfg = @bitCast(((menvcfg & mask) ^ (menvcfg & mask)) | value);
                        },
                        .X64 => self.csrs.menvcfg = @bitCast(value),
                    },
                    CSRAddr.menvcfgh.to_u12() => if (arch == .X32) {
                        const mask: u64 = 0xffffffff << 32;
                        const menvcfg: u64 = @bitCast(self.csrs.menvcfg);
                        self.csrs.menvcfg = @bitCast(((menvcfg & mask) ^ (menvcfg & mask)) | (@as(u64, value) << 32));
                    },

                    CSRAddr.pmpcfg0.to_u12() => self.csrs.pmpcfg0 = @bitCast(value),
                    CSRAddr.pmpaddr0.to_u12() => self.csrs.pmpaddr0 = value,

                    // CSRAddr.mnscratch.to_u12() => self.csrs.mnscratch = value,
                    // CSRAddr.mnepc.to_u12() => self.csrs.mnepc = value,
                    // CSRAddr.mncause.to_u12() => self.csrs.mncause = @bitCast(value),
                    // CSRAddr.mnstatus.to_u12() => self.csrs.mnstatus = @bitCast(value),

                    else => {
                        print("CSR_STORE: Unknown CSR\n", .{});
                        return error.UnknownCSR;
                    },
                }
            }

            fn csr_load(self: *@This(), csr_addr: u12) !uarch {
                switch (csr_addr) {
                    CSRAddr.time.to_u12() => return @truncate(self.csrs.time),

                    CSRAddr.sstatus.to_u12() => return @bitCast(self.csrs.sstatus),
                    CSRAddr.sie.to_u12() => return self.csrs.sie,
                    CSRAddr.stvec.to_u12() => return @bitCast(self.csrs.stvec),
                    CSRAddr.scounteren.to_u12() => return @as(u32, @bitCast(self.csrs.scounteren)),

                    CSRAddr.sscratch.to_u12() => return self.csrs.sscratch,
                    CSRAddr.sepc.to_u12() => return self.csrs.sepc,
                    CSRAddr.scause.to_u12() => return @bitCast(self.csrs.scause),
                    CSRAddr.stval.to_u12() => return self.csrs.stval,
                    CSRAddr.sip.to_u12() => return @bitCast(self.csrs.sip),

                    CSRAddr.satp.to_u12() => return @bitCast(self.csrs.satp),

                    CSRAddr.stimecmp.to_u12() => return @truncate(self.csrs.stimecmp),

                    CSRAddr.mvendorid.to_u12() => return self.csrs.mvendorid,
                    CSRAddr.marchid.to_u12() => return self.csrs.marchid,
                    CSRAddr.mimpid.to_u12() => return self.csrs.mimpid,
                    CSRAddr.mhartid.to_u12() => return self.csrs.mhartid,
                    CSRAddr.mconfigptr.to_u12() => return self.csrs.mconfigptr,

                    CSRAddr.mstatus.to_u12() => return @bitCast(self.csrs.mstatus),
                    CSRAddr.misa.to_u12() => return @bitCast(self.csrs.misa),
                    CSRAddr.mideleg.to_u12() => return self.csrs.mideleg,
                    CSRAddr.medeleg.to_u12() => return self.csrs.medeleg,
                    CSRAddr.mie.to_u12() => return @bitCast(self.csrs.mie),
                    CSRAddr.mtvec.to_u12() => return @bitCast(self.csrs.mtvec),

                    CSRAddr.mscratch.to_u12() => return self.csrs.mscratch,
                    CSRAddr.mepc.to_u12() => return self.csrs.mepc,
                    CSRAddr.mcause.to_u12() => return @bitCast(self.csrs.mcause),
                    CSRAddr.mtval.to_u12() => return self.csrs.mtval,
                    CSRAddr.mcounteren.to_u12() => return @as(u32, @bitCast(self.csrs.mcounteren)),
                    CSRAddr.mip.to_u12() => return @bitCast(self.csrs.mip),
                    CSRAddr.mtinst.to_u12() => return self.csrs.mtinst,
                    CSRAddr.mtval2.to_u12() => return self.csrs.mtval2,

                    CSRAddr.menvcfg.to_u12() => return @truncate(@as(u64, @bitCast(self.csrs.menvcfg))),
                    CSRAddr.menvcfgh.to_u12() => if (arch == .X32) {
                        return @truncate(@as(u64, @bitCast(self.csrs.menvcfg)) >> 32);
                    } else {
                        return 0;
                    },

                    CSRAddr.pmpcfg0.to_u12() => return @bitCast(self.csrs.pmpcfg0),
                    CSRAddr.pmpaddr0.to_u12() => return self.csrs.pmpaddr0,
                    else => {
                        print("CSR_LOAD: Unknown CSR\n", .{});
                        return error.UnknownCSR;
                    },

                    // CSRAddr.mnscratch.to_u12() => return self.csrs.mnscratch,
                    // CSRAddr.mnepc.to_u12() => return self.csrs.mnepc,
                    // CSRAddr.mncause.to_u12() => return @bitCast(self.csrs.mncause),
                    // CSRAddr.mnstatus.to_u12() => return @bitCast(self.csrs.mnstatus),
                }
            }

            fn get_pmpcfg_from_paddri(self: @This(), pmpaddr_i: u6) u8 {
                const div =
                    switch (arch) {
                    .X32 => 4,
                    .X64 => 8,
                };
                const csr_offset_i = pmpaddr_i / div;
                const seg = pmpaddr_i % div;

                const csr_offset = switch (arch) {
                    .X32 => csr_offset_i,
                    .X64 => csr_offset_i * 2,
                };

                const csr_addr = CSRAddr.pmpcfg0.to_u12() + csr_offset;

                return @truncate(self.csrs[csr_addr] >> (seg * 8));
            }

            // RVI

            fn lui(self: *@This(), instr: InstrFX32) void {
                const u = instr.u;
                if (u.rd != 0) {
                    self.g_regs[u.rd] = 0;
                    self.g_regs[u.rd] = @bitCast(@as(iarch, @as(i32, @bitCast((@as(u32, u.imm_31_12) << 12)))));
                }
                self.pc += 4;
            }

            fn auipc(self: *@This(), instr: InstrFX32) void {
                const offset: uarch = @bitCast(@as(iarch, @as(i32, @bitCast(@as(u32, instr.u.imm_31_12) << 12))));
                if (instr.u.rd != 0) {
                    self.g_regs[instr.u.rd] = self.pc +% offset;
                }
                self.pc += 4;
            }

            fn jal(self: *@This(), instr: InstrFX32) void {
                const pc = self.pc + 4;
                self.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(self.pc)) + (@as(iarch, @as(i21, @bitCast(rearrange(u20, u21, instr.jimm.imm, &base.@"imm_20|10:1|11|19:12")))))));
                if (instr.u.rd != 0) {
                    self.g_regs[instr.u.rd] = pc;
                }
            }

            fn jalr(self: *@This(), instr: InstrFX32) void {
                const pc = self.pc + 4;
                self.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(self.g_regs[instr.i.rs1])) + (@as(iarch, @as(i12, @bitCast(instr.i.imm_11_0))))));
                self.pc ^= self.pc & 1;
                if (instr.i.rd != 0) {
                    self.g_regs[instr.i.rd] = pc;
                }
            }

            fn branch(self: *@This(), instr: InstrFX32) void {
                const offset = @as(iarch, instr.b.get_imm());
                switch (instr.b.funct3) {
                    0b000 => { // BEQ
                        if (self.g_regs[instr.b.rs1] == self.g_regs[instr.b.rs2]) {
                            self.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(self.pc)) + offset));
                            return;
                        }
                    },
                    0b001 => { // BNE
                        if (self.g_regs[instr.b.rs1] != self.g_regs[instr.b.rs2]) {
                            self.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(self.pc)) + offset));
                            return;
                        }
                    },
                    0b100 => { // BLT
                        if (@as(iarch, @bitCast(self.g_regs[instr.b.rs1])) < @as(iarch, @bitCast(self.g_regs[instr.b.rs2]))) {
                            self.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(self.pc)) + offset));
                            return;
                        }
                    },
                    0b101 => { // BGE
                        if (@as(iarch, @bitCast(self.g_regs[instr.b.rs1])) >= @as(iarch, @bitCast(self.g_regs[instr.b.rs2]))) {
                            self.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(self.pc)) + offset));
                            return;
                        }
                    },
                    0b110 => { // BLTU
                        if (self.g_regs[instr.b.rs1] < self.g_regs[instr.b.rs2]) {
                            self.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(self.pc)) + offset));
                            return;
                        }
                    },
                    0b111 => { // BGEU
                        if (self.g_regs[instr.b.rs1] >= self.g_regs[instr.b.rs2]) {
                            self.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(self.pc)) + offset));
                            return;
                        }
                    },
                    else => {
                        print("Invalid BRANCH func3: {b:0>3}", .{instr.b.funct3});
                    },
                }
                self.pc += 4;
            }

            fn load(self: *@This(), instr: InstrFX32, cpu: *CPU) !void {
                const offset: uarch = @bitCast(@as(iarch, @bitCast(self.g_regs[instr.i.rs1])) + @as(iarch, instr.i.imm_11_0));
                switch (instr.i.funct3) {
                    0b000 => { // LB
                        var buffer: [1]u8 = undefined;
                        try self.mmio_read(cpu, offset, &buffer);
                        self.g_regs[instr.i.rd] = @bitCast(@as(iarch, std.mem.readInt(i8, &buffer, .little)));
                    },
                    0b001 => { // LH
                        var buffer: [2]u8 = undefined;
                        try self.mmio_read(cpu, offset, &buffer);
                        self.g_regs[instr.i.rd] = @bitCast(@as(iarch, std.mem.readInt(i16, &buffer, .little)));
                    },
                    0b010 => { // LW
                        var buffer: [4]u8 = undefined;
                        try self.mmio_read(cpu, offset, &buffer);
                        self.g_regs[instr.i.rd] = @bitCast(@as(iarch, std.mem.readInt(i32, &buffer, .little)));
                    },
                    0b100 => { // LBU
                        var buffer: [1]u8 = undefined;
                        try self.mmio_read(cpu, offset, &buffer);
                        self.g_regs[instr.i.rd] = std.mem.readInt(u8, &buffer, .little);
                    },
                    0b101 => { // LHU
                        var buffer: [2]u8 = undefined;
                        try self.mmio_read(cpu, offset, &buffer);
                        self.g_regs[instr.i.rd] = std.mem.readInt(u16, &buffer, .little);
                    },
                    0b110 => { // LWU
                        if (uarch == u64) {
                            var buffer: [4]u8 = undefined;
                            try self.mmio_read(cpu, offset, &buffer);
                            self.g_regs[instr.i.rd] = std.mem.readInt(u32, &buffer, .little);
                        } else {
                            print("LWD is not implemented for x32 CPU!\n", .{});
                        }
                    },
                    0b011 => { // LD
                        if (uarch == u64) {
                            var buffer: [8]u8 = undefined;
                            try self.mmio_read(cpu, offset, &buffer);
                            self.g_regs[instr.i.rd] = @bitCast(std.mem.readInt(i64, &buffer, .little));
                        } else {
                            print("LD is not implemented for x32 CPU!\n", .{});
                        }
                    },
                    else => {
                        print("Invalid LOAD func3: {b:0>3}\n", .{instr.b.funct3});
                    },
                }
                self.pc += 4;
            }

            fn store(self: *@This(), instr: InstrFX32, cpu: *CPU) !void {
                const offset: uarch = self.g_regs[instr.s.rs1] +% @as(uarch, @bitCast(@as(iarch, instr.s.get_imm())));
                var buffer: [bits / 8]u8 = undefined;
                std.mem.writeInt(uarch, &buffer, self.g_regs[instr.s.rs2], .little);
                switch (instr.s.funct3) {
                    0b000 => { // SB
                        try self.mmio_write(cpu, offset, buffer[0..1]);
                    },
                    0b001 => { // SH
                        try self.mmio_write(cpu, offset, buffer[0..2]);
                    },
                    0b010 => { // SW
                        try self.mmio_write(cpu, offset, buffer[0..4]);
                    },
                    0b011 => { // SD
                        if (uarch == u64) {
                            try self.mmio_write(cpu, offset, buffer[0..8]);
                        } else {
                            print("SD is not implemented for x32 CPU!\n", .{});
                        }
                    },
                    else => {
                        print("Invalid STORE func3: {b:0>3}\n", .{instr.b.funct3});
                    },
                }
                self.pc += 4;
            }

            fn op_imm(self: *@This(), instr: InstrFX32) void {
                const i = instr.i;
                switch (instr.i.funct3) {
                    0b000 => { // ADDI
                        if (i.rd != 0) {
                            self.g_regs[i.rd] = self.g_regs[i.rs1] +% @as(uarch, @bitCast(@as(iarch, i.imm_11_0)));
                        }
                    },
                    0b010 => { // SLTI
                        if (i.rd != 0) {
                            self.g_regs[i.rd] = if (@as(iarch, @bitCast(self.g_regs[i.rs1])) < @as(iarch, i.imm_11_0)) 1 else 0;
                        }
                    },
                    0b011 => { // SLTIU
                        if (i.rd != 0) {
                            self.g_regs[i.rd] = if (i.rs1 == 0) 1 else if (self.g_regs[i.rs1] < @as(uarch, @bitCast(@as(iarch, i.imm_11_0)))) 1 else 0;
                        }
                    },
                    0b100 => { // XORI
                        if (i.rd != 0) {
                            self.g_regs[i.rd] = @as(uarch, @bitCast(@as(iarch, @bitCast(self.g_regs[i.rs1])) ^ @as(iarch, i.imm_11_0)));
                        }
                    },
                    0b110 => { // ORI
                        if (i.rd != 0) {
                            self.g_regs[i.rd] = @as(uarch, @bitCast(@as(iarch, @bitCast(self.g_regs[i.rs1])) | @as(iarch, i.imm_11_0)));
                        }
                    },
                    0b111 => { // ANDI
                        if (i.rd != 0) {
                            self.g_regs[i.rd] = @as(uarch, @bitCast(@as(iarch, @bitCast(self.g_regs[i.rs1])) & @as(iarch, i.imm_11_0)));
                        }
                    },
                    0b001 => { // SLLI
                        switch (instr.i_1.op) {
                            0b000000 => {
                                if (instr.i_1.rd != 0) {
                                    self.g_regs[instr.i_1.rd] = self.g_regs[instr.i_1.rs1] << @truncate(instr.i_1.shamt);
                                }
                            },
                            else => {
                                print("Special I, not implemented: {b:0>6}\n", .{instr.i_1.op});
                            },
                        }
                    },
                    0b101 => { // SRLI/SRAI
                        switch (instr.i_1.op) {
                            0b000000 => { // SRLI
                                if (i.rd != 0) {
                                    self.g_regs[i.rd] = self.g_regs[i.rs1] >> @truncate(instr.i_1.shamt);
                                }
                            },
                            0b010000 => { // SRAI
                                if (i.rd != 0) {
                                    const mask = (@as(uarch, std.math.maxInt(uarch)) >> @truncate(instr.i_1.shamt)) ^ @as(uarch, std.math.maxInt(uarch));
                                    self.g_regs[i.rd] = (self.g_regs[i.rs1] >> @truncate(instr.i_1.shamt)) | (mask & if (self.g_regs[i.rs1] & 1 << (bits - 1) == 1 << (bits - 1)) @as(uarch, std.math.maxInt(uarch)) else 0);
                                }
                            },
                            else => {
                                print("Special I, not implemented: {b:0>6}\n", .{instr.i_1.op});
                            },
                        }
                    },
                }
                self.pc += 4;
            }

            fn op_imm_32(self: *@This(), instr: InstrFX32) void {
                self.pc += 4;
                if (uarch == u32) {
                    print("OP-IMM-32 not implemented for x32\n", .{});
                    return;
                }
                const i = instr.i;
                switch (i.funct3) {
                    0b000 => { // ADDIW
                        if (i.rd != 0) {
                            self.g_regs[i.rd] = @bitCast(@as(i64, @as(i32, @truncate(@as(iarch, @bitCast(self.g_regs[i.rs1])))) +% @as(i32, i.imm_11_0)));
                        }
                    },
                    0b001 => { // SLLIW
                        if (i.rd != 0) {
                            self.g_regs[i.rd] = @as(u64, @bitCast(@as(i64, @as(i32, @bitCast(@as(u32, @truncate(self.g_regs[i.rs1])) << @truncate(instr.i_1.shamt))))));
                        }
                    },
                    0b101 => { // SRLIW/SRAIW
                        switch (instr.i_1.op) {
                            0b000000 => { // SRLIW
                                if (i.rd != 0) {
                                    self.g_regs[i.rd] = @as(u64, @bitCast(@as(i64, @as(i32, @bitCast(@as(u32, @truncate(self.g_regs[i.rs1])) >> @truncate(instr.i_1.shamt))))));
                                }
                            },
                            0b010000 => { // SRAIW
                                if (i.rd != 0) {
                                    const mask = (@as(u32, std.math.maxInt(u32)) >> @truncate(instr.i_1.shamt)) ^ @as(u32, std.math.maxInt(u32));
                                    self.g_regs[i.rd] = @bitCast(@as(i64, @as(i32, @bitCast((@as(u32, @truncate(self.g_regs[i.rs1])) >> @truncate(instr.i_1.shamt)) | (mask & if (self.g_regs[i.rs1] & 1 << (31) == 1 << (31)) @as(u32, std.math.maxInt(u32)) else 0)))));
                                }
                            },
                            else => {
                                print("Special I, not implemented: {b:0>6}\n", .{instr.i_1.op});
                            },
                        }
                    },
                    else => {
                        print("OP-IMM-32 func3 not implemented: {b:0>3}\n", .{i.funct3});
                    },
                }
            }

            fn op(self: *@This(), instr: InstrFX32) !void {
                const r = instr.r;
                switch (r.funct7) {
                    0b0000000 => {
                        switch (r.funct3) {
                            0b000 => { // ADD
                                if (r.rd != 0) {
                                    self.g_regs[r.rd] = self.g_regs[r.rs1] +% self.g_regs[r.rs2];
                                }
                            },
                            0b001 => { // SLL
                                if (r.rd != 0) {
                                    self.g_regs[r.rd] = self.g_regs[r.rs1] << @as(stype, @truncate(self.g_regs[r.rs2]));
                                }
                            },
                            0b010 => { // SLT
                                if (r.rd != 0) {
                                    self.g_regs[r.rd] = if (@as(iarch, @bitCast(self.g_regs[r.rs1])) < @as(iarch, @bitCast(self.g_regs[r.rs2]))) 1 else 0;
                                }
                            },
                            0b011 => { // SLTU
                                if (r.rd != 0) {
                                    self.g_regs[r.rd] = if (r.rs1 == 0 and r.rs2 != 0) 1 else if (self.g_regs[r.rs1] < self.g_regs[r.rs2]) 1 else 0;
                                }
                            },
                            0b100 => { // XOR
                                if (r.rd != 0) {
                                    self.g_regs[r.rd] = self.g_regs[r.rs1] ^ self.g_regs[r.rs2];
                                }
                            },
                            0b101 => { // SRL
                                if (r.rd != 0) {
                                    self.g_regs[r.rd] = self.g_regs[r.rs1] >> @as(stype, @truncate(self.g_regs[r.rs2]));
                                }
                            },
                            0b110 => { // OR
                                if (r.rd != 0) {
                                    self.g_regs[r.rd] = self.g_regs[r.rs1] | self.g_regs[r.rs2];
                                }
                            },
                            0b111 => { // AND
                                if (r.rd != 0) {
                                    self.g_regs[r.rd] = self.g_regs[r.rs1] & self.g_regs[r.rs2];
                                }
                            },
                        }
                    },
                    0b0000001 => { // RV32M
                        switch (r.funct3) {
                            0b000 => { // MUL
                                self.g_regs[r.rd] = self.g_regs[r.rs1] * self.g_regs[r.rs2];
                            },
                            0b001 => { // MULH
                                return error.NotImplemented;
                            },
                            0b010 => { // MULHSU
                                return error.NotImplemented;
                            },
                            0b011 => { // MULHU
                                return error.NotImplemented;
                            },
                            0b100 => { // DIV
                                return error.NotImplemented;
                            },
                            0b101 => { // DIVU
                                return error.NotImplemented;
                            },
                            0b110 => { // REM
                                return error.NotImplemented;
                            },
                            0b111 => { // REMU
                                return error.NotImplemented;
                            },
                        }
                    },
                    0b0100000 => {
                        switch (r.funct3) {
                            0b000 => { // SUB
                                if (r.rd != 0) {
                                    self.g_regs[r.rd] = self.g_regs[r.rs1] -% self.g_regs[r.rs2];
                                }
                            },
                            0b101 => { // SRA
                                if (r.rd != 0) {
                                    const mask = (@as(uarch, std.math.maxInt(uarch)) >> @as(stype, @truncate(self.g_regs[r.rs2]))) ^ @as(uarch, std.math.maxInt(uarch));
                                    self.g_regs[r.rd] = (self.g_regs[r.rs1] >> @as(stype, @truncate(self.g_regs[r.rs2]))) | (mask & if (self.g_regs[r.rs1] & 1 << (bits - 1) == 1 << (bits - 1)) @as(uarch, std.math.maxInt(uarch)) else 0);
                                }
                            },
                            else => {
                                print("Invalid funct3 for OP {b:0>3}\n", .{r.funct3});
                                return error.NotImplemented;
                            },
                        }
                    },
                    else => {
                        print("Invalid funct7 for OP {b:0>7}\n", .{r.funct7});
                        return error.NotImplemented;
                    },
                }
                self.pc += 4;
            }

            fn op_32(self: *@This(), instr: InstrFX32) void {
                self.pc += 4;
                if (uarch == u32) {
                    print("OP-32 not implemented for x32\n", .{});
                    return;
                }

                const r = instr.r;
                switch (r.funct3) {
                    0b000 => { // ADDW/SUBW
                        switch (r.funct7) {
                            0b0000000 => { // ADDW
                                if (r.rd != 0) {
                                    self.g_regs[r.rd] = @bitCast(@as(i64, @as(i32, @truncate(@as(i64, @bitCast(self.g_regs[r.rs1])))) +% @as(i32, @truncate(@as(i64, @bitCast(self.g_regs[r.rs2]))))));
                                }
                            },
                            0b0100000 => { // SUBW
                                if (r.rd != 0) {
                                    self.g_regs[r.rd] = @bitCast(@as(i64, @as(i32, @truncate(@as(i64, @bitCast(self.g_regs[r.rs1])))) -% @as(i32, @truncate(@as(i64, @bitCast(self.g_regs[r.rs2]))))));
                                }
                            },
                            else => {
                                print("Invalid funct7 for ADD or SUB {b:0>7}\n", .{r.funct7});
                            },
                        }
                    },
                    0b001 => { // SLLW
                        if (r.rd != 0) {
                            self.g_regs[r.rd] = @bitCast(@as(i64, @as(i32, @bitCast(@as(u32, @truncate(self.g_regs[r.rs1])) << @truncate(self.g_regs[r.rs2])))));
                        }
                    },
                    0b101 => { // SRLW/SRAW
                        switch (r.funct7) {
                            0b0000000 => { // SRLW
                                if (r.rd != 0) {
                                    self.g_regs[r.rd] = @bitCast(@as(i64, @as(i32, @bitCast(@as(u32, @truncate(self.g_regs[r.rs1])) >> @truncate(self.g_regs[r.rs2])))));
                                }
                            },
                            0b0100000 => { // SRAW
                                if (r.rd != 0) {
                                    const mask = (@as(u32, std.math.maxInt(u32)) >> @truncate(self.g_regs[r.rs2])) ^ @as(u32, std.math.maxInt(u32));
                                    self.g_regs[r.rd] = @bitCast(@as(i64, @as(i32, @bitCast((@as(u32, @truncate(self.g_regs[r.rs1])) >> @truncate(self.g_regs[r.rs2])) | (mask & if (self.g_regs[r.rs1] & 1 << 31 == 1 << 31) @as(u32, std.math.maxInt(u32)) else 0)))));
                                }
                            },
                            else => {
                                print("Invalid funct7 for SRL or SRA {b:0>7}\n", .{r.funct7});
                            },
                        }
                    },
                    else => {
                        print("Invalid func3 for OP-32 {b:0>3}\n", .{r.funct3});
                    },
                }
            }

            fn misc_mem(self: *@This(), instr: InstrFX32) void {
                switch (instr.f.func3) {
                    0b000 => {
                        print("FENCE is doing nothing!\n", .{});
                    },
                    else => {
                        print("Invalid func3 for MISC-MEM {b:0>3}\n", .{instr.f.func3});
                    },
                }
                self.pc += 4;
            }

            fn system(self: *@This(), instr: InstrFX32, cpu: *CPU) !void {
                _ = cpu;
                switch (instr.i.funct3) {
                    0b000 => switch (instr.i.imm_11_0) {
                        0 => { // ECALL
                            switch (self.mode) {
                                .U => {
                                    std.debug.assert(self.trap(CSRS.CAUSE.ECallFromU)); // Environment call from U-mode
                                    return;
                                },
                                .S => {
                                    std.debug.assert(self.trap(CSRS.CAUSE.ECallFromS)); // Environment call from S-mode
                                    return;
                                },
                                .M => {
                                    std.debug.assert(self.trap(CSRS.CAUSE.ECallFromM)); // Environment call from M-mode
                                    return;
                                },
                                else => {
                                    @panic("ECALL is not implemented!");
                                },
                            }
                        },
                        1 => { // EBREAK
                            return error.Break;
                        },
                        0b000100000010 => { // SRET
                            if (self.mode != .S and self.mode != .M) {
                                std.debug.print("SRET: HartMode is not M or S, is {s}\n", .{self.mode.name()});
                                self.illegal_instruction();
                                return;
                            }
                            self.pc = self.csrs.sepc;
                            const sstatus = &self.csrs.sstatus;
                            sstatus.SIE = sstatus.SPIE;
                            self.mode = HartMode.from_u2(sstatus.SPP);
                            sstatus.SPIE = 1;
                            sstatus.SPP = @truncate(HartMode.U.to_u2());
                        },
                        0b001100000010 => { // MRET
                            if (self.mode != .M) {
                                std.debug.print("MRET: HartMode is not M, is {s}\n", .{self.mode.name()});
                                self.illegal_instruction();
                                return;
                            }
                            self.pc = self.csrs.mepc;
                            const mstatus = &self.csrs.mstatus;
                            mstatus.MIE = mstatus.MPIE;
                            self.mode = HartMode.from_u2(mstatus.MPP);
                            mstatus.MPIE = 1;
                            mstatus.MPP = HartMode.U.to_u2();
                            return;
                        },
                        0b011100000010 => { // MNRET
                            if (self.mode != .M) {
                                std.debug.print("MNRET: HartMode is not M, is {s}\n", .{self.mode.name()});
                                self.illegal_instruction();
                                return;
                            }
                            self.pc = self.csrs.mnepc;
                            const mnstatus = &self.csrs.mnstatus;
                            self.mode = HartMode.from_u2(mnstatus.MNPP);
                            if (self.mode != .M) {
                                mnstatus.NMIE = 1;
                            }
                            mnstatus.MNPP = HartMode.U.to_u2();
                            return;
                        },
                        else => {
                            print("Invalid SYSTEM call {b:0>12}\n", .{instr.i.imm_11_0});
                            @panic("TODO: Not Implemented");
                        },
                    },
                    // CSRRW
                    0b001 => {
                        const csr_addr: u12 = @bitCast(instr.i.imm_11_0);
                        const per = has_csr_permisions(self.mode, csr_addr);
                        if (!(per.read() and per.write())) {
                            self.illegal_instruction();
                            return;
                        }
                        if (instr.i.rd != 0) {
                            self.g_regs[instr.i.rd] = self.csr_load(csr_addr) catch {
                                self.illegal_instruction();
                                return;
                            };
                        }
                        self.csr_store(csr_addr, self.g_regs[instr.i.rs1]) catch {
                            self.illegal_instruction();
                            return;
                        };
                    },
                    // CSRRS
                    0b010 => {
                        const csr_addr: u12 = @bitCast(instr.i.imm_11_0);
                        const per = has_csr_permisions(self.mode, csr_addr);
                        const tmp = self.csr_load(csr_addr) catch {
                            self.illegal_instruction();
                            return;
                        };
                        if (instr.i.rd != 0) {
                            if (!per.read()) {
                                self.illegal_instruction();
                                return;
                            }
                            self.g_regs[instr.i.rd] = tmp;
                        }
                        if (instr.i.rs1 != 0) {
                            if (!per.write()) {
                                self.illegal_instruction();
                                return;
                            }
                            self.csr_store(csr_addr, tmp & self.g_regs[instr.i.rs1]) catch {
                                self.illegal_instruction();
                                return;
                            };
                        }
                    },
                    // CSRRC
                    0b011 => {
                        const csr_addr: u12 = @bitCast(instr.i.imm_11_0);
                        const per = has_csr_permisions(self.mode, csr_addr);
                        const tmp = self.csr_load(csr_addr) catch {
                            self.illegal_instruction();
                            return;
                        };
                        if (instr.i.rd != 0) {
                            if (!per.read()) {
                                self.illegal_instruction();
                                return;
                            }
                            self.g_regs[instr.i.rd] = tmp;
                        }
                        if (instr.i.rs1 != 0) {
                            if (!per.write()) {
                                self.illegal_instruction();
                                return;
                            }
                            self.csr_store(csr_addr, tmp ^ (self.g_regs[instr.i.rs1] & tmp)) catch {
                                self.illegal_instruction();
                                return;
                            };
                        }
                    },
                    // CSRRWI
                    0b101 => {
                        const csr_addr: u12 = @bitCast(instr.i.imm_11_0);
                        const per = has_csr_permisions(self.mode, csr_addr);
                        const value: uarch = @as(u5, @bitCast(instr.i.rs1));
                        if (!(per.read() and per.write())) {
                            self.illegal_instruction();
                            return;
                        }
                        if (instr.i.rd != 0) {
                            self.g_regs[instr.i.rd] = self.csr_load(csr_addr) catch {
                                self.illegal_instruction();
                                return;
                            };
                        }
                        self.csr_store(csr_addr, value) catch {
                            self.illegal_instruction();
                            return;
                        };
                    },
                    // CSRRSI
                    0b110 => {
                        const csr_addr: u12 = @bitCast(instr.i.imm_11_0);
                        const per = has_csr_permisions(self.mode, csr_addr);
                        const value: uarch = @as(u5, @bitCast(instr.i.rs1));
                        const tmp = self.csr_load(csr_addr) catch {
                            self.illegal_instruction();
                            return;
                        };
                        if (!(per.read() and per.write())) {
                            self.illegal_instruction();
                            return;
                        }
                        if (instr.i.rd == 0) {
                            self.g_regs[instr.i.rd] = tmp;
                        }
                        if (value != 0) {
                            self.csr_store(csr_addr, tmp & value) catch {
                                self.illegal_instruction();
                                return;
                            };
                        }
                    },
                    // CSRRCI
                    0b111 => {
                        const csr_addr: u12 = @bitCast(instr.i.imm_11_0);
                        const per = has_csr_permisions(self.mode, csr_addr);
                        const value: uarch = @as(u5, @bitCast(instr.i.rs1));
                        const tmp = self.csr_load(csr_addr) catch {
                            self.illegal_instruction();
                            return;
                        };
                        if (!(per.read() and per.write())) {
                            self.illegal_instruction();
                            return;
                        }
                        if (instr.i.rd != 0) {
                            self.g_regs[instr.i.rd] = tmp;
                        }
                        if (value != 0) {
                            self.csr_store(csr_addr, tmp ^ (value & tmp)) catch {
                                self.illegal_instruction();
                                return;
                            };
                        }
                    },
                    else => {
                        print("Invalid SYSTEM funct3 {b:0>3}\n", .{instr.i.funct3});
                    },
                }

                self.pc += 4;
            }

            // END RVI

            // RVA

            fn amo(self: *@This(), instr: InstrFX32, cpu: *CPU) !void {
                if (instr.r.funct3 == 0b010) {
                    switch (instr.r.funct7 >> 2) {
                        0b00001 => { // amoswap.w
                            var buffer: [4]u8 = undefined;
                            try self.mmio_read(cpu, self.g_regs[instr.r.rs1], &buffer);
                            self.g_regs[instr.r.rd] = @bitCast(@as(iarch, std.mem.readInt(i32, &buffer, .little)));
                            std.mem.writeInt(i32, &buffer, @truncate(@as(iarch, @bitCast(self.g_regs[instr.r.rs2]))), .little);
                            try self.mmio_write(cpu, self.g_regs[instr.r.rs1], &buffer);
                        },
                        else => {},
                    }
                }

                std.debug.print("Atomic instruction not implemented\n", .{});
                instr.debug();
                self.pc += 4;
            }

            // END RVA

            // RVC

            fn c_ldsp(self: *@This(), instr: InstrFX16, cpu: *CPU) !void {
                const offset: uarch = (@as(u9, instr.ci.imm_12) << 5) | rearrange(u5, u9, instr.ci.imm_2_6, &base.@"imm_4:3|8:6");
                var buffer: [bits / 8]u8 = undefined;
                switch (arch) {
                    .X32 => {
                        return error.NotImplemented;
                    },
                    .X64 => {
                        try self.mmio_read(cpu, self.g_regs[2] +% offset, &buffer);
                    },
                }
                self.g_regs[instr.ci.rd] = std.mem.readInt(uarch, &buffer, .little);
                self.pc += 2;
            }

            fn c_sdsp(self: *@This(), instr: InstrFX16, cpu: *CPU) !void {
                const offset: uarch = rearrange(u6, u9, instr.css.imm, &base.@"imm_5:3|8:6");
                var buffer: [bits / 8]u8 = undefined;
                std.mem.writeInt(uarch, &buffer, self.g_regs[instr.css.rs2], .little);
                switch (arch) {
                    .X32 => {
                        return error.NotImplemented;
                    },
                    .X64 => {
                        try self.mmio_write(cpu, self.g_regs[2] +% offset, &buffer);
                    },
                }
                self.pc += 2;
            }

            fn c_addi4spn(self: *@This(), instr: InstrFX16) void {
                self.g_regs[instr.ciw.rd()] = self.g_regs[2] +% @as(uarch, rearrange(u8, u10, instr.ciw.imm, &base.@"imm_5:4|9:6|2|3"));
                self.pc += 2;
            }

            fn c_addi(self: *@This(), instr: InstrFX16) void {
                const imm = @as(iarch, @as(i6, @bitCast((@as(u6, instr.ci.imm_12) << 5) | @as(u6, instr.ci.imm_2_6))));

                if (instr.ci.rd == 0) {
                    self.pc += 2;
                    return;
                }

                self.g_regs[instr.ci.rd] +%= @as(uarch, @bitCast(imm));
                self.pc += 2;
            }

            fn c_addiw(self: *@This(), instr: InstrFX16) void {
                if (arch == .X32) {
                    print("C_ADDIW is not implemented for x32 CPU!\n", .{});
                    self.pc += 2;
                    return;
                }

                const imm = @as(i32, @as(i6, @bitCast((@as(u6, instr.ci.imm_12) << 5) | @as(u6, instr.ci.imm_2_6))));
                self.g_regs[instr.ci.rd] = @bitCast(@as(iarch, @as(i32, @bitCast(@as(u32, @truncate(self.g_regs[instr.ci.rd])) +% @as(u32, @bitCast(imm))))));
                self.pc += 2;
            }

            fn c_lui(self: *@This(), instr: InstrFX16) void {
                if (instr.ci.rd == 0) {
                    self.pc += 2;
                    return;
                }

                if (instr.ci.rd == 2) {
                    const imm = @as(uarch, @bitCast(@as(iarch, @as(i10, @bitCast(rearrange(u5, u10, instr.ci.imm_2_6, &base.@"imm_4|6|8:7|5") | @as(u10, instr.ci.imm_12) << 9)))));

                    if (imm == 0) {
                        @panic("imm 0 in c.lui");
                    }

                    self.g_regs[instr.ci.rd] +%= imm;
                } else {
                    const imm = @as(iarch, @as(i6, @bitCast((@as(u6, instr.ci.imm_12) << 5) | @as(u6, instr.ci.imm_2_6))));

                    if (imm == 0) {
                        @panic("imm 0 in c.lui");
                    }

                    self.g_regs[instr.ci.rd] = @as(uarch, @bitCast(imm)) << 12;
                }
                self.pc += 2;
            }

            fn c_li(self: *@This(), instr: InstrFX16) void {
                if (instr.ci.rd != 0) {
                    const offset = @as(iarch, @as(i6, @bitCast((@as(u6, instr.ci.imm_12) << 5) | @as(u6, instr.ci.imm_2_6))));
                    self.g_regs[instr.ci.rd] = @bitCast(offset);
                }
                self.pc += 2;
            }

            fn c_jr_mv(self: *@This(), instr: InstrFX16) !void {
                if (instr.cr.rs2 == 0) {
                    self.pc = self.g_regs[instr.cr.rd];
                } else {
                    if (instr.cr.rd != 0) {
                        self.g_regs[instr.cr.rd] = self.g_regs[instr.cr.rs2];
                    }
                    self.pc += 2;
                }
            }

            fn c_add(self: *@This(), instr: InstrFX16) void {
                if (instr.cr.rd != 0) {
                    self.g_regs[instr.cr.rd] +%= self.g_regs[instr.cr.rs2];
                }
                self.pc += 2;
            }

            fn c_slli(self: *@This(), instr: InstrFX16) void {
                const offset = if (arch != .X32) (@as(u6, instr.ci.imm_12) << 5) else 0 | @as(stype, instr.ci.imm_2_6);
                if (instr.ci.rd != 0) {
                    if (offset != 0) {
                        self.g_regs[instr.ci.rd] <<= offset;
                    } else {
                        self.g_regs[instr.ci.rd] <<= bits / 2;
                        self.g_regs[instr.ci.rd] <<= bits / 2;
                    }
                }
                self.pc += 2;
            }

            fn c_srli(self: *@This(), instr: InstrFX16) void {
                const offset = if (arch != .X32) (@as(u6, instr.ci.imm_12) << 5) else 0 | @as(stype, instr.ci.imm_2_6);
                if (offset != 0) {
                    self.g_regs[instr.ci.rd] >>= offset;
                } else {
                    self.g_regs[instr.ci.rd] >>= bits / 2;
                    self.g_regs[instr.ci.rd] >>= bits / 2;
                }
                self.pc += 2;
            }

            fn c_or(self: *@This(), instr: InstrFX16) void {
                if (instr.ca.rd() != 0) {
                    self.g_regs[instr.ca.rd()] |= self.g_regs[instr.ca.rs2()];
                }
                self.pc += 2;
            }

            fn c_and(self: *@This(), instr: InstrFX16) void {
                if (instr.ca.rd() != 0) {
                    self.g_regs[instr.ca.rd()] &= self.g_regs[instr.ca.rs2()];
                }
                self.pc += 2;
            }

            fn c_andi(self: *@This(), instr: InstrFX16) void {
                const imm: i6 = @bitCast((@as(u6, instr.cb.offset2 >> 2) << 5) | @as(u6, instr.cb.offset1));
                self.g_regs[instr.cb.rd()] = self.g_regs[instr.cb.rd()] & @as(uarch, @bitCast(@as(iarch, imm)));

                self.pc += 2;
            }

            fn c_beqz(self: *@This(), instr: InstrFX16) void {
                const imm = rearrange(u3, u9, instr.cb.offset2, &base.@"imm_8|4:3") | rearrange(u5, u9, instr.cb.offset1, &base.@"imm_7:6|2:1|5");
                const offset = @as(uarch, @bitCast(@as(iarch, @as(i9, @bitCast(imm)))));
                if (self.g_regs[instr.cb.rd()] == 0) {
                    self.pc = self.pc +% offset;
                } else {
                    self.pc += 2;
                }
            }

            fn c_bnez(self: *@This(), instr: InstrFX16) void {
                const imm = rearrange(u3, u9, instr.cb.offset2, &base.@"imm_8|4:3") | rearrange(u5, u9, instr.cb.offset1, &base.@"imm_7:6|2:1|5");
                const offset = @as(uarch, @bitCast(@as(iarch, @as(i9, @bitCast(imm)))));
                if (self.g_regs[instr.cb.rd()] != 0) {
                    self.pc = self.pc +% offset;
                } else {
                    self.pc += 2;
                }
            }

            fn c_lw(self: *@This(), instr: InstrFX16, cpu: *CPU) !void {
                var buffer: [4]u8 = undefined;
                const imm = rearrange(u3, u8, instr.cs.imm2, &base.@"imm_5:3") | rearrange(u2, u8, instr.cs.imm1, &base.@"imm_2|6");
                const offset = @as(uarch, imm);
                try self.mmio_read(cpu, self.g_regs[instr.cl.rs1()] +% offset, &buffer);
                self.g_regs[instr.cl.rd()] = std.mem.readInt(u32, &buffer, .little);
                self.pc += 2;
            }

            fn c_sw(self: *@This(), instr: InstrFX16, cpu: *CPU) !void {
                var buffer: [4]u8 = undefined;
                std.mem.writeInt(u32, &buffer, @truncate(self.g_regs[instr.cs.rs2()]), .little);
                const imm = rearrange(u3, u8, instr.cs.imm2, &base.@"imm_5:3") | rearrange(u2, u8, instr.cs.imm1, &base.@"imm_2|6");
                const offset = @as(uarch, imm);
                try self.mmio_write(cpu, self.g_regs[instr.cs.rs1()] +% offset, &buffer);
                self.pc += 2;
            }

            fn c_sd(self: *@This(), instr: InstrFX16, cpu: *CPU) !void {
                if (arch == .X64) {
                    var buffer: [8]u8 = undefined;
                    std.mem.writeInt(u64, &buffer, @truncate(self.g_regs[instr.cs.rs2()]), .little);
                    const imm = rearrange(u3, u8, instr.cs.imm2, &base.@"imm_5:3") | rearrange(u2, u8, instr.cs.imm1, &base.@"imm_7:6");
                    const offset = @as(uarch, imm);
                    try self.mmio_write(cpu, self.g_regs[instr.cs.rs1()] +% offset, &buffer);
                    self.pc += 2;
                } else {
                    print("C_SD is not implemented for x32 CPU!\n", .{});
                    return;
                }
            }

            fn c_j(self: *@This(), instr: InstrFX16) void {
                const imm = base.rearrange(u11, u12, @bitCast(instr.cj.target), &base.@"imm_11|4|9:8|10|6|7|3:1|5");
                const offset: uarch = @bitCast(@as(iarch, imm));
                self.pc = self.pc +% offset;
                if (offset == 0) {
                    print("Panic dectected!\n", .{});
                    @breakpoint();
                }
            }

            // END RVC

            pub fn mmio_read(self: *@This(), cpu: *CPU, virtual: uarch, buffer: []u8) !void {
                const address = try self.map_to_memory(cpu, virtual);
                try cpu.mmio_read(address, buffer);
            }

            pub fn mmio_write(self: *@This(), cpu: *CPU, virtual: uarch, buffer: []const u8) !void {
                const address = try self.map_to_memory(cpu, virtual);
                try cpu.mmio_write(address, buffer);
            }

            pub fn map_to_memory(self: *@This(), cpu: *CPU, virtual: uarch) !uarch {
                switch (self.mode) {
                    .H, .M => {
                        return virtual;
                    },
                    .S, .U => switch (arch) {
                        .X32 => switch (self.csrs.satp.MODE) {
                            0 => { // Bare
                                return virtual;
                            },
                            1 => { // Sv32
                                const root = @as(u32, self.csrs.satp.PPN) * std.math.pow(u32, 2, 10) * 4;
                                print("Root: 0x{x}\n", .{root});
                                const Sv32PTE = packed struct { V: u1, R: u1, W: u1, X: u1, U: u1, G: u1, A: u1, D: u1, RSW: u2, PPN0: u10, PPN1: u12 };

                                var pte: Sv32PTE = undefined;
                                try cpu.mmio_read(root, std.mem.asBytes(&pte));
                                print("PTE: {}\n", .{pte});

                                if (pte.V == 1) {
                                    if (pte.R == 0 and pte.W == 0 and pte.X == 0) // Leaf
                                    {
                                        const next_addr = ((@as(u32, pte.PPN1) << 10) | @as(u32, pte.PPN0)) * std.math.pow(u32, 2, 10) * 4;
                                        print("NEXT: 0x{x}\n", .{next_addr});

                                        try cpu.mmio_read(next_addr, std.mem.asBytes(&pte));
                                        print("PTE: {}\n", .{pte});
                                        // This is uninitialized memory, why?

                                        // The Root is 0x80005000 and NEXT is 0x80006000
                                        // the address looks valid but at that point is uninitialized memory or 0xaaaa
                                    }
                                }

                                @panic("Not implemented");
                            },
                        },
                        .X64 => switch (self.csrs.satp.MODE) {
                            0 => { // Bare
                                return virtual;
                            },
                            else => {
                                @panic("Not implemented");
                            },
                        },
                    },
                }
            }
        };

        harts: [harts_len]Hart,
        allocator: Allocator,
        bus: std.ArrayListUnmanaged(BusEntry),

        pub fn init(allocator: Allocator) !CPU {
            var cpu = CPU{ .allocator = allocator, .harts = std.mem.zeroes([harts_len]Hart), .bus = .{} };

            for (&cpu.harts, 0..) |*hart, i| {
                hart.mode = .M;
                hart.csrs.mhartid = @truncate(i);
            }

            return cpu;
        }

        pub fn deinit(self: *CPU) void {
            self.bus.deinit(self.allocator);
        }

        pub fn add_mmio(self: *CPU, comptime TYPE: type, start: u64, io: *TYPE) !void {
            comptime {
                const size_fn = @typeInfo(@TypeOf(TYPE.size)).Fn;
                const read_fn = @typeInfo(@TypeOf(TYPE.read)).Fn;
                const write_fn = @typeInfo(@TypeOf(TYPE.write)).Fn;
                if (size_fn.params[0].type.? != *TYPE or size_fn.return_type.? != u64) {
                    @compileLog(size_fn);
                    @compileError("Invalid size function for io");
                }
                if (read_fn.params[0].type.? != *TYPE or read_fn.params[1].type.? != u64 or read_fn.params[2].type.? != []u8) {
                    @compileError("Invalid read function for io");
                }
                if (write_fn.params[0].type.? != *TYPE or write_fn.params[1].type.? != u64 or write_fn.params[2].type.? != []const u8) {
                    @compileLog(write_fn);
                    @compileError("Invalid write function for io");
                }
            }
            try self.add_mmio_entry(.{
                .start = start,
                .end = start + TYPE.size(io) - 1,
                .io = io,
                .fn_write = @ptrCast(&TYPE.write),
                .fn_read = @ptrCast(&TYPE.read),
            });
        }

        pub fn add_mmio_entry(self: *CPU, new_entry: BusEntry) !void {
            if (new_entry.start > new_entry.end) {
                return error.StartIsBiggerThenEnd;
            }

            try self.bus.append(self.allocator, new_entry);
        }

        pub fn mmio_read(self: CPU, address: uarch, buffer: []u8) !void {
            for (self.bus.items) |entry| {
                if (entry.start <= address and address + buffer.len <= entry.end) {
                    entry.read(address - entry.start, buffer);
                    return;
                }
            }
            return error.CannotRead;
        }

        pub fn mmio_write(self: *CPU, address: u64, buffer: []const u8) !void {
            for (self.bus.items) |entry| {
                if (entry.start <= address and address <= entry.end) {
                    entry.write(address - entry.start, buffer);
                    return;
                }
            }
            return error.CannotWrite;
        }
    };
}

test "CPU" {
    const testing = std.testing;
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var cpu = try buildCPU(.X64, 1).init(gpa.allocator());
    defer cpu.deinit();

    print("Res: {}\n", .{cpu.harts[0].get_pmpcfg_from_paddri(0)});
    try testing.expect(false);
}
