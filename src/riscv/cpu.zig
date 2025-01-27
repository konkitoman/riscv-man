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

const print = std.debug.print;

/// # Has inclusive ranges!!!
pub const MemoryMap = struct {
    start: u64,
    end: u64,
    to_start: u64,
    to_end: u64,

    pub fn is_in_virtual_range(self: @This(), address: u64) bool {
        return self.to_start <= address and address <= self.to_end;
    }

    pub fn is_in_range(self: @This(), address: u64) bool {
        return self.start <= address and address <= self.end;
    }
};

pub fn buildCPU(comptime arch: Arch, comptime harts_len: usize) type {
    const uarch = arch.uarch();
    const iarch = arch.iarch();
    const bits: comptime_int = @typeInfo(uarch).Int.bits;
    const stype: type = switch (arch) {
        .X32 => u5,
        .X64 => u6,
    };

    return struct {
        const CPU = @This();

        pub const UARCH = uarch;

        pub const EEI = struct {
            ecall: *const fn (hart: *Hart, cpu: *CPU) void = default_ecall,

            pub fn default_ecall(hart: *Hart, cpu: *CPU) void {
                print("ECALL not implemented!\n", .{});
                _ = hart;
                _ = cpu;
            }
        };

        pub const CSR = struct {
            data: uarch,
            read: *const fn (self: *CSR) uarch,
            write: *const fn (self: *CSR, value: uarch) void,
        };

        pub const CSRU = struct {
            o_csr: ?CSR,

            pub fn read(self: *@This()) !uarch {
                return if (self.o_csr) |*csr| csr.read(csr) else {
                    return error.NotImplemented;
                };
            }

            pub fn write(self: *@This(), value: uarch) !void {
                if (self.o_csr) |*csr| csr.write(csr, value) else {
                    return error.NotImplemented;
                }
            }

            pub fn set(self: *@This(), value: uarch) !void {
                const tmp = try self.read();
                try self.write(tmp | value);
            }

            pub fn clear(self: *@This(), value: uarch) !void {
                const tmp = try self.read();
                try self.write(tmp ^ (tmp & value));
            }
        };

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

        pub const Hart = struct {
            g_regs: [32]uarch,
            pc: uarch,
            csrs: [4096]CSRU,
            mode: HartMode,

            pub fn step(self: *Hart, cpu: *CPU) !void {
                var memory: [8]u8 = undefined;
                const memory_len = try cpu.vmemory_read(self.pc, &memory);
                if (memory_len == 0) {
                    return error.EndOfStream;
                }
                const e_var_instr = VarInstr.from_memory(memory[0..memory_len]);

                if (e_var_instr) |var_instr| switch (var_instr) {
                    .x16 => |x16| {
                        const instr = InstrFX16.from_u16(x16);
                        switch (instr.opcode) {
                            0b00 => switch (instr.cl.funct3) {
                                0b000 => self.c_addi4spn(instr),
                                else => {
                                    var_instr.debug();
                                    return error.NotImplemented;
                                },
                            },
                            0b01 => if (instr.ci.rd != 0)
                                switch (instr.ci.funct3) {
                                    0b000 => self.c_addi(instr),
                                    0b001 => self.c_addiw(instr),
                                    0b010 => self.c_li(instr),
                                    0b011 => self.c_lui(instr),
                                    0b100 => switch (@as(u2, @truncate(instr.cb.offset2))) {
                                        0b00 => self.c_srli(instr),
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
                                    else => {
                                        var_instr.debug();
                                        return error.NotImplemented;
                                    },
                                },
                            0b10 => switch (instr.ci.funct3) {
                                0b000 => self.c_slli(instr),
                                0b011 => try self.c_ldsp(instr, cpu),
                                0b100 => switch (instr.ci.imm_12) {
                                    0 => try self.c_jr(instr),
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
                            0b1101111 => self.jal(instr),
                            0b1100111 => self.jalr(instr),
                            0b1100011 => self.branch(instr),
                            0b0000011 => try self.load(instr, cpu),
                            0b0100011 => try self.save(instr, cpu),
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

            fn has_csr_permisions(self: @This(), addr: u12) Permisions {
                switch (self.mode) {
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
                    },
                }

                return .None;
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
                self.pc = @as(uarch, @bitCast(@as(iarch, @bitCast(self.pc)) + (@as(iarch, instr.j.get_imm()) * 2)));
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
                const offset = @as(iarch, instr.b.get_imm()) * 2;
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
                        try cpu.vmemory_read_all(offset, &buffer);
                        self.g_regs[instr.i.rd] = @bitCast(@as(iarch, std.mem.readInt(i8, &buffer, .little)));
                    },
                    0b001 => { // LH
                        var buffer: [2]u8 = undefined;
                        try cpu.vmemory_read_all(offset, &buffer);
                        self.g_regs[instr.i.rd] = @bitCast(@as(iarch, std.mem.readInt(i16, &buffer, .little)));
                    },
                    0b010 => { // LW
                        var buffer: [4]u8 = undefined;
                        try cpu.vmemory_read_all(offset, &buffer);
                        self.g_regs[instr.i.rd] = @bitCast(@as(iarch, std.mem.readInt(i32, &buffer, .little)));
                    },
                    0b100 => { // LBU
                        var buffer: [1]u8 = undefined;
                        try cpu.vmemory_read_all(offset, &buffer);
                        self.g_regs[instr.i.rd] = std.mem.readInt(u8, &buffer, .little);
                    },
                    0b101 => { // LHU
                        var buffer: [2]u8 = undefined;
                        try cpu.vmemory_read_all(offset, &buffer);
                        self.g_regs[instr.i.rd] = std.mem.readInt(u16, &buffer, .little);
                    },
                    0b110 => { // LWU
                        if (uarch == u64) {
                            var buffer: [4]u8 = undefined;
                            try cpu.vmemory_read_all(offset, &buffer);
                            self.g_regs[instr.i.rd] = std.mem.readInt(u32, &buffer, .little);
                        } else {
                            print("LWD is not implemented for x32 CPU!\n", .{});
                        }
                    },
                    0b011 => { // LD
                        if (uarch == u64) {
                            var buffer: [8]u8 = undefined;
                            try cpu.vmemory_read_all(offset, &buffer);
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

            fn save(self: *@This(), instr: InstrFX32, cpu: *CPU) !void {
                const offset: uarch = @bitCast((@as(iarch, @bitCast(self.g_regs[instr.s.rs1])) + instr.s.get_imm()));
                var buffer: [bits / 8]u8 = undefined;
                std.mem.writeInt(uarch, &buffer, self.g_regs[instr.s.rs2], .little);
                switch (instr.s.funct3) {
                    0b000 => { // SB
                        try cpu.vmemory_write_all(offset, buffer[0..1]);
                    },
                    0b001 => { // SH
                        try cpu.vmemory_write_all(offset, buffer[0..2]);
                    },
                    0b010 => { // SW
                        try cpu.vmemory_write_all(offset, buffer[0..4]);
                    },
                    0b011 => { // SD
                        if (uarch == u64) {
                            try cpu.vmemory_write_all(offset, buffer[0..8]);
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
                switch (instr.i.funct3) {
                    0b000 => switch (instr.i.imm_11_0) {
                        // ECALL
                        0 => cpu.eei.ecall(self, cpu),
                        1 => { // EBREAK
                            return error.Break;
                        },
                        else => {
                            print("Invalid SYSTEM call {b:0>12}\n", .{instr.i.imm_11_0});
                        },
                    },
                    // CSRRW
                    0b001 => {
                        const csr_addr: u12 = @bitCast(instr.i.imm_11_0);
                        const per = self.has_csr_permisions(csr_addr);
                        if (instr.i.rd != 0 and per.read()) {
                            self.g_regs[instr.i.rd] = try self.csrs[csr_addr].read();
                        }
                        if (per.write()) {
                            _ = try self.csrs[csr_addr].write(self.g_regs[instr.i.rs1]);
                        }
                    },
                    // CSRRS
                    0b010 => {
                        const csr_addr: u12 = @bitCast(instr.i.imm_11_0);
                        const per = self.has_csr_permisions(csr_addr);
                        if (instr.i.rd != 0 and per.read()) {
                            self.g_regs[instr.i.rd] = try self.csrs[csr_addr].read();
                        }
                        if (instr.i.rs1 != 0 and per.write()) {
                            _ = try self.csrs[csr_addr].set(self.g_regs[instr.i.rs1]);
                        }
                    },
                    // CSRRC
                    0b011 => {
                        const csr_addr: u12 = @bitCast(instr.i.imm_11_0);
                        const per = self.has_csr_permisions(csr_addr);
                        if (instr.i.rd != 0 and per.read()) {
                            self.g_regs[instr.i.rd] = try self.csrs[csr_addr].read();
                        }
                        if (instr.i.rs1 != 0 and per.write()) {
                            _ = try self.csrs[csr_addr].clear(self.g_regs[instr.i.rs1]);
                        }
                    },
                    // CSRRWI
                    0b101 => {
                        const csr_addr: u12 = @bitCast(instr.i.imm_11_0);
                        const per = self.has_csr_permisions(csr_addr);
                        const value: uarch = @as(u5, @bitCast(instr.i.rs1));
                        if (instr.i.rd != 0 and per.read()) {
                            self.g_regs[instr.i.rd] = try self.csrs[csr_addr].read();
                        }
                        if (per.write()) {
                            _ = try self.csrs[csr_addr].write(value);
                        }
                    },
                    // CSRRSI
                    0b110 => {
                        const csr_addr: u12 = @bitCast(instr.i.imm_11_0);
                        const per = self.has_csr_permisions(csr_addr);
                        const value: uarch = @as(u5, @bitCast(instr.i.rs1));
                        if (instr.i.rd == 0 and per.read()) {
                            self.g_regs[instr.i.rd] = try self.csrs[csr_addr].read();
                        }
                        if (value != 0 and per.write()) {
                            _ = try self.csrs[csr_addr].set(value);
                        }
                    },
                    // CSRRCI
                    0b111 => {
                        const csr_addr: u12 = @bitCast(instr.i.imm_11_0);
                        const per = self.has_csr_permisions(csr_addr);
                        const value: uarch = @as(u5, @bitCast(instr.i.rs1));
                        if (instr.i.rd != 0 and per.read()) {
                            self.g_regs[instr.i.rd] = try self.csrs[csr_addr].read();
                        }
                        if (value != 0 and per.write()) {
                            _ = try self.csrs[csr_addr].clear(value);
                        }
                    },
                    else => {
                        print("Invalid SYSTEM funct3 {b:0>3}\n", .{instr.i.funct3});
                    },
                }

                self.pc += 4;
            }

            // END RVI

            // RVC

            fn c_ldsp(self: *@This(), instr: InstrFX16, cpu: *CPU) !void {
                const offset: uarch = (@as(u6, instr.ci.imm_12) << 5) | instr.ci.imm_2_6;
                var buffer: [bits / 8]u8 = undefined;
                switch (arch) {
                    .X32 => {
                        return error.NotImplemented;
                        // try cpu.vmemory_read_all(self.g_regs[2] +% (offset * 4), &buffer);
                    },
                    .X64 => {
                        try cpu.vmemory_read_all(self.g_regs[2] +% (offset * 8), &buffer);
                    },
                }
                self.g_regs[instr.ci.rd] = std.mem.readInt(uarch, &buffer, .little);
                self.pc += 2;
            }

            fn c_sdsp(self: *@This(), instr: InstrFX16, cpu: *CPU) !void {
                const offset: uarch = instr.css.imm;
                var buffer: [bits / 8]u8 = undefined;
                std.mem.writeInt(uarch, &buffer, self.g_regs[instr.css.rs2], .little);
                switch (arch) {
                    .X32 => {
                        return error.NotImplemented;
                        // try cpu.vmemory_write_all(self.g_regs[2] +% (offset * 4), &buffer);
                    },
                    .X64 => {
                        try cpu.vmemory_write_all(self.g_regs[2] +% (offset * 8), &buffer);
                    },
                }
                self.pc += 2;
            }

            fn c_addi4spn(self: *@This(), instr: InstrFX16) void {
                self.g_regs[instr.ciw.rd()] = self.g_regs[2] +% @as(uarch, instr.ciw.imm);
                self.pc += 2;
            }

            fn c_addi(self: *@This(), instr: InstrFX16) void {
                const imm = @as(iarch, @as(i6, @bitCast((@as(u6, instr.ci.imm_12) << 5) | @as(u6, instr.ci.imm_2_6))));

                if (imm == 0) {
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
                const imm = @as(iarch, @as(i6, @bitCast((@as(u6, instr.ci.imm_12) << 5) | @as(u6, instr.ci.imm_2_6))));
                if (instr.ci.rd == 2) {
                    if (imm == 0) {
                        self.pc += 2;
                        return;
                    }
                    self.g_regs[2] +%= @as(uarch, @bitCast(imm * 16));
                } else {
                    self.g_regs[instr.ci.rd] = @as(uarch, @bitCast(imm)) << 12;
                }
                self.pc += 2;
            }

            fn c_li(self: *@This(), instr: InstrFX16) void {
                const offset = @as(iarch, @as(i6, @bitCast((@as(u6, instr.ci.imm_12) << 5) | @as(u6, instr.ci.imm_2_6))));
                self.g_regs[instr.ci.rd] = @bitCast(offset);
                self.pc += 2;
            }

            fn c_jr(self: *@This(), instr: InstrFX16) !void {
                if (instr.cr.rd == 0) {
                    return error.NotImplemented;
                } else {
                    self.g_regs[instr.cr.rd] = self.g_regs[instr.cr.rs2];
                }
                self.pc += 2;
            }

            fn c_add(self: *@This(), instr: InstrFX16) void {
                self.g_regs[instr.cr.rd] +%= self.g_regs[instr.cr.rs2];
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
                self.g_regs[instr.cr.rd] |= self.g_regs[instr.cr.rs2];
                self.pc += 2;
            }

            fn c_and(self: *@This(), instr: InstrFX16) void {
                self.g_regs[instr.cr.rd] &= self.g_regs[instr.cr.rs2];
                self.pc += 2;
            }

            // END RVC
        };

        eei: EEI,
        harts: [harts_len]Hart,
        allocator: Allocator,
        memory_maps: std.ArrayList(MemoryMap),
        memory: std.ArrayList(u8),

        pub const CSRDeclarator = struct {
            csr_addr: u12,
            initial_csr: CSR,
        };

        pub fn init(allocator: Allocator, csr_declarators: []const CSRDeclarator, eei: EEI) !CPU {
            var cpu = CPU{
                .allocator = allocator,
                .harts = std.mem.zeroes([harts_len]Hart),
                .memory_maps = std.ArrayList(MemoryMap).init(allocator),
                .memory = std.ArrayList(u8).init(allocator),
                .eei = eei,
            };

            for (&cpu.harts, 0..) |*hart, i| {
                for (csr_declarators) |csr_declarator| {
                    hart.csrs[csr_declarator.csr_addr].o_csr = csr_declarator.initial_csr;
                }

                hart.mode = .M;

                _ = try hart.csrs[CSRAddr.mhartid.to_u12()].write(@truncate(i));
                _ = try hart.csrs[CSRAddr.mvendorid.to_u12()].write(0);
                _ = try hart.csrs[CSRAddr.marchid.to_u12()].write(0);
            }

            return cpu;
        }

        pub fn deinit(self: *CPU) void {
            self.memory_maps.deinit();
            self.memory.deinit();
        }

        pub fn get_memory_size(self: CPU) u64 {
            return self.memory.items.len;
        }

        pub fn set_memory_size(self: *CPU, size: u64) !void {
            try self.memory.resize(@as(usize, size));
            self.memory.items.len = size;
        }

        pub fn add_memory_map(self: *CPU, memory_map: MemoryMap) !void {
            if (memory_map.start > memory_map.end) {
                return error.StartIsBiggerThenEnd;
            }

            if (memory_map.start >= self.memory.items.len or memory_map.end > self.memory.items.len) {
                return error.OutOfSpace;
            }

            for (self.memory_maps.items) |map| {
                if (memory_map.end >= map.start and memory_map.end <= map.end) {
                    return error.OverlappingMemory;
                }

                if (memory_map.to_end >= map.to_start and memory_map.to_end <= map.to_end) {
                    return error.OverlappingVirtualMemory;
                }
            }

            try self.memory_maps.append(memory_map);
        }

        pub fn vmemory_read(self: CPU, address: uarch, buffer: []u8) !u64 {
            var written: u64 = 0;
            for (0..buffer.len) |i| {
                const offset = self.map_to_memory(address + i) catch break;
                buffer[i] = self.memory.items[offset];
                written += 1;
            }
            return written;
        }

        pub fn vmemory_read_all(self: CPU, address: uarch, buffer: []u8) !void {
            for (0..buffer.len) |i| {
                const offset = try self.map_to_memory(address + i);
                buffer[i] = self.memory.items[offset];
            }
        }

        pub fn vmemory_write(self: *CPU, address: u64, buffer: []const u8) !u64 {
            var written: u64 = 0;
            for (0..buffer.len) |i| {
                const offset = self.map_to_memory(address + i) catch break;
                self.memory.items[offset] = buffer[i];
                written += 1;
            }
            return written;
        }

        pub fn vmemory_write_all(self: *CPU, address: u64, buffer: []const u8) !void {
            for (0..buffer.len) |i| {
                const offset = try self.map_to_memory(address + i);
                self.memory.items[offset] = buffer[i];
            }
        }

        pub fn map_to_memory(self: CPU, address: u64) !u64 {
            for (self.memory_maps.items) |map| {
                if (map.is_in_virtual_range(address)) {
                    return (address - map.to_start) + map.start;
                }
            }
            return error.OutOfSpace;
        }

        pub fn map_to_virtual_memory(self: CPU, address: u64) !u64 {
            for (self.memory_maps.items) |map| {
                if (map.is_in_range(address)) {
                    return (address - map.start) + map.to_start;
                }
            }
            return error.OutOfSpace;
        }

        pub fn step(self: *CPU) !void {
            for (&self.harts) |*thread| {
                try thread.step(self);
            }
        }

        pub const CSRSMachine = struct {
            fn r_generic(self: *CSR) uarch {
                return self.data;
            }
            fn w_generic(self: *CSR, value: uarch) void {
                self.data = value;
            }

            const DEFAULT_CSR = CSR{ .data = 0, .read = &r_generic, .write = &w_generic };

            pub const VENDORID: CSRDeclarator = .{ .csr_addr = CSRAddr.mvendorid.to_u12(), .initial_csr = DEFAULT_CSR };
            pub const ARCHID: CSRDeclarator = .{ .csr_addr = CSRAddr.marchid.to_u12(), .initial_csr = DEFAULT_CSR };
            pub const IMPID: CSRDeclarator = .{ .csr_addr = CSRAddr.mimpid.to_u12(), .initial_csr = DEFAULT_CSR };
            pub const HARTID: CSRDeclarator = .{ .csr_addr = CSRAddr.mhartid.to_u12(), .initial_csr = DEFAULT_CSR };
            pub const MSTATUS: CSRDeclarator = .{ .csr_addr = CSRAddr.mstatus.to_u12(), .initial_csr = DEFAULT_CSR };
            pub const MEPC: CSRDeclarator = .{ .csr_addr = CSRAddr.mepc.to_u12(), .initial_csr = DEFAULT_CSR };
            pub const MEDELEG: CSRDeclarator = .{ .csr_addr = CSRAddr.medeleg.to_u12(), .initial_csr = DEFAULT_CSR };
            pub const MIDELEG: CSRDeclarator = .{ .csr_addr = CSRAddr.mideleg.to_u12(), .initial_csr = DEFAULT_CSR };
            pub const PMPADDR0: CSRDeclarator = .{ .csr_addr = CSRAddr.pmpaddr0.to_u12(), .initial_csr = DEFAULT_CSR };
            pub const PMPCFG0: CSRDeclarator = .{ .csr_addr = CSRAddr.pmpcfg0.to_u12(), .initial_csr = DEFAULT_CSR };
            pub const MIE: CSRDeclarator = .{ .csr_addr = CSRAddr.mie.to_u12(), .initial_csr = DEFAULT_CSR };
            pub const MENVCFG: CSRDeclarator = .{ .csr_addr = CSRAddr.menvcfg.to_u12(), .initial_csr = DEFAULT_CSR };
            pub const MCOUNTEREN: CSRDeclarator = .{ .csr_addr = CSRAddr.mcounteren.to_u12(), .initial_csr = DEFAULT_CSR };
            pub const MTVEC: CSRDeclarator = .{ .csr_addr = CSRAddr.mtvec.to_u12(), .initial_csr = DEFAULT_CSR };
            pub const MNSTATUS: CSRDeclarator = .{ .csr_addr = CSRAddr.mnstatus.to_u12(), .initial_csr = DEFAULT_CSR };
            pub const MSCRATCH: CSRDeclarator = .{ .csr_addr = CSRAddr.mscratch.to_u12(), .initial_csr = DEFAULT_CSR };

            pub fn build_default(addr: u12) CSRDeclarator {
                return CSRDeclarator{ .csr_addr = addr, .initial_csr = DEFAULT_CSR };
            }

            pub const DEFAULT = .{
                CPU.CSRSMachine.HARTID,
                CPU.CSRSMachine.VENDORID,
                CPU.CSRSMachine.ARCHID,
                CPU.CSRSMachine.IMPID,
                CPU.CSRSMachine.MSTATUS,
                CPU.CSRSMachine.MEPC,
                CPU.CSRSMachine.MEDELEG,
                CPU.CSRSMachine.MIDELEG,
                CPU.CSRSMachine.PMPADDR0,
                CPU.CSRSMachine.PMPCFG0,
                CPU.CSRSMachine.MIE,
                CPU.CSRSMachine.MENVCFG,
                CPU.CSRSMachine.MCOUNTEREN,
                CPU.CSRSMachine.MTVEC,
                CPU.CSRSMachine.MNSTATUS,
                CPU.CSRSMachine.MSCRATCH,
                CPU.CSRSMachine.build_default(0x10),
            };

            pub const CSRS = [_]CSRDeclarator{ VENDORID, ARCHID, IMPID, HARTID, MSTATUS };
        };
    };
}
