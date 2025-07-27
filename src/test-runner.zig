const std = @import("std");
const base = @import("riscv/base.zig");
const default_EEI = @import("riscv/default_EEI.zig");
const riscv_asm = @import("riscv/asm.zig");
const elf = @import("elf.zig");
const Allocator = std.mem.Allocator;
const IOMemory = @import("io/memory.zig");

const I = @import("riscv/extension/I.zig");
const Zifencei = @import("riscv/extension/Zifencei.zig");
const Zicsr = @import("riscv/extension/Zicsr.zig");

const Arch = base.Arch;

const DataEEI = default_EEI.DataEEI;

const print = std.debug.print;

pub fn main() !void {
    var args = std.process.args();
    const path = args.next();
    _ = path;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var cpu_meta = if (args.next()) |bin| bin else {
        print("cpu meta is required!\n", .{});
        return;
    };

    var arch: u8 = undefined;
    {
        var o_arch: ?u8 = null;

        if (cpu_meta.len >= 4) select: {
            if (std.mem.eql(u8, cpu_meta[0..2], "RV")) {
                if (std.mem.eql(u8, cpu_meta[2..4], "32")) {
                    o_arch = 32;
                    cpu_meta = cpu_meta[4..];
                    break :select;
                } else if (std.mem.eql(u8, cpu_meta[2..4], "64")) {
                    o_arch = 64;
                    cpu_meta = cpu_meta[4..];
                    break :select;
                }
            }
            return error.CPUMetaInvalid;
        }

        arch = o_arch orelse {
            return error.CPUMetaNoArch;
        };
    }

    const program_path = if (args.next()) |bin| bin else {
        print("program path is required!\n", .{});
        return;
    };

    switch (arch) {
        32 => {
            var inner = try build(.X32).init(gpa.allocator(), program_path);
            while (true) {
                try inner.step();
            }
        },
        64 => {
            var inner = try build(.X64).init(gpa.allocator(), program_path);
            while (true) {
                try inner.step();
            }
        },
        else => {
            unreachable;
        },
    }
}

const IOTOHOST = struct {
    result: u64,

    pub fn size(self: *@This()) u64 {
        _ = self;
        return 8;
    }

    pub fn read(self: *@This(), index: u64, buffer: []u8) void {
        @memcpy(buffer, std.mem.asBytes(&self.result)[index .. index + buffer.len]);
    }

    pub fn write(self: *@This(), index: u64, buffer: []const u8) void {
        @memcpy(std.mem.asBytes(&self.result)[index .. index + buffer.len], buffer);
    }
};

pub fn build(comptime ARCH: Arch) type {
    const ASM = riscv_asm.build_asm(ARCH);
    return struct {
        const DataHart = struct {
            const uarch = ARCH.uarch();

            I: I.buildDataHart(ARCH),
            Zicsr: Zicsr.buildDataHart(ARCH),

            const CAUSE = Zicsr.buildDataHart(ARCH).CAUSE;

            pub fn read(self: *@This(), eei_data: *DataEEI, index: u64, buffer: []u8) bool {
                switch (self.Zicsr.xlen) {
                    .X32 => {
                        const satp = @as(Zicsr.X32SATP, @bitCast(@as(u32, @truncate(self.Zicsr.satp))));
                        switch (self.Zicsr.mode) {
                            .M => eei_data.mmio_read(index, buffer),
                            .S => {
                                if (satp.MODE == 0) {
                                    eei_data.mmio_read(index, buffer);
                                    return true;
                                }
                                @panic("Supervisor");
                            },
                            .U => {
                                if (satp.MODE == 0) {
                                    eei_data.mmio_read(index, buffer);
                                    return true;
                                }
                                @panic("User");
                            },
                            else => std.debug.panic("Uimplemented mode {}\n", .{self.Zicsr.mode}),
                        }
                    },
                    .X64 => {
                        if (ARCH == .X32) unreachable;
                        const satp = @as(Zicsr.X64SATP, @bitCast(@as(u64, @truncate(self.Zicsr.satp))));
                        switch (self.Zicsr.mode) {
                            .M => eei_data.mmio_read(index, buffer),
                            .S => {
                                if (satp.MODE == 0) {
                                    eei_data.mmio_read(index, buffer);
                                    return true;
                                }

                                @panic("Supervisor");
                            },
                            .U => {
                                if (satp.MODE == 0) {
                                    eei_data.mmio_read(index, buffer);
                                    return true;
                                }
                                @panic("User");
                            },
                            else => std.debug.panic("Uimplemented mode {}\n", .{self.Zicsr.mode}),
                        }
                    },
                }

                return true;
            }

            pub fn write(self: *@This(), eei_data: *DataEEI, index: u64, buffer: []const u8) bool {
                _ = self;

                eei_data.mmio_write(index, buffer);

                return true;
            }

            pub fn fence(self: *@This(), eei_data: *DataEEI) void {
                _ = eei_data;

                std.debug.print("FENCE not implemented\n", .{});
                self.I.pc += 4;
            }

            pub fn ecall(self: *@This(), eei_data: *DataEEI) void {
                _ = eei_data;

                self.m_trap(CAUSE.ECallFromU);
            }

            pub fn ebreak(self: *@This(), eei_data: *DataEEI) void {
                _ = eei_data;

                std.debug.print("EBREAK not implemented\n", .{});
                self.I.pc += 4;
            }

            pub fn illegal_instruction(self: *@This()) void {
                std.debug.print("Illegal Instruction\n", .{});
                self.m_trap(CAUSE.IllegalInstruction);
            }

            fn trap(self: *@This(), cause: CAUSE) bool {
                if (cause.interrupt == 1) @panic("Interrupt not implemented");

                switch (self.mode) {
                    .U, .S => {
                        if (self.Zicsr.medeleg >> @truncate(cause.code) & 1 == 1) {
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

            fn m_trap(self: *@This(), cause: CAUSE) void {
                self.Zicsr.xlen = ARCH;

                switch (ARCH) {
                    .X32 => {
                        const mstatus = @as(*Zicsr.X32MSTATUS, @ptrCast(&self.Zicsr.mstatus));
                        mstatus.MPP = self.Zicsr.mode.to_u2();
                        self.Zicsr.mode = .M;
                        self.Zicsr.mepc = self.I.pc;
                        self.Zicsr.mcause = cause;
                        const tvec = @as(Zicsr.X32TVEC, @bitCast(self.Zicsr.mtvec));
                        switch (tvec.mode) {
                            0 => { // DIRECT
                                self.I.pc = @as(uarch, tvec.base) << 2;
                            },
                            1 => { // VECTORED
                                // TODO: Implement VECTORED
                                @panic("VECTORED not implemented!");
                                // self.I.pc = (@as(uarch, tvec.base) << 2) +% (@as(uarch, cause.code) * 4);
                            },
                            else => {
                                @panic("Unknown MTVEC Mode!");
                            },
                        }
                    },
                    .X64 => {
                        const mstatus = @as(*Zicsr.X64MSTATUS, @ptrCast(&self.Zicsr.mstatus));
                        mstatus.MPP = self.Zicsr.mode.to_u2();
                        self.Zicsr.mode = .M;
                        self.Zicsr.mepc = self.I.pc;
                        self.Zicsr.mcause = cause;
                        const tvec = @as(Zicsr.X64TVEC, @bitCast(self.Zicsr.mtvec));
                        switch (tvec.mode) {
                            0 => { // DIRECT
                                self.I.pc = @as(uarch, tvec.base) << 2;
                            },
                            1 => { // VECTORED
                                // TODO: Implement VECTORED
                                @panic("VECTORED not implemented!");
                                // self.I.pc = (@as(uarch, tvec.base) << 2) +% (@as(uarch, cause.code) * 4);
                            },
                            else => {
                                @panic("Unknown MTVEC Mode!");
                            },
                        }
                    },
                }
            }

            fn s_trap(self: *@This(), cause: CAUSE) void {
                self.Zicsr.sstatus.SPP = @truncate(self.Zicsr.mode.to_u2());
                self.Zicsr.mode = .S;
                self.Zicsr.sepc = self.I.pc;
                self.Zicsr.scause = cause;
                const tvec = self.Zicsr.stvec;
                switch (tvec.mode) {
                    0 => { // DIRECT
                        self.I.pc = @as(uarch, tvec.base) << 2;
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
        };

        const INSTRS = I.buildInstrs(ARCH, DataEEI, DataHart) ++ Zifencei.buildInstrs(ARCH, DataEEI, DataHart) ++ Zicsr.buildInstrs(ARCH, DataEEI, DataHart);
        const EEI = default_EEI.buildEEI(ARCH, 1, DataHart, &INSTRS);
        const ELF = elf.build(EEI);

        allocator: Allocator,
        io_tohost: *IOTOHOST,
        memory: [8]u8,
        io_memory: *IOMemory,
        cpu: *EEI,

        pub fn init(allocator: Allocator, program_path: []const u8) !@This() {
            const cpu = try allocator.create(EEI);
            errdefer allocator.destroy(cpu);
            cpu.* = EEI.init(allocator, std.mem.zeroInit(DataHart, .{}));
            errdefer cpu.deinit();

            const memory = try allocator.alloc(u8, 1024 * 1000 * 2); // 2MB
            errdefer allocator.free(memory);

            const io_memory = try allocator.create(IOMemory);
            errdefer allocator.destroy(io_memory);

            io_memory.* = IOMemory.init(memory);

            try cpu.data.mmio_add(IOMemory, 0x80000000, io_memory);

            // Loading ELF
            var file = try std.fs.cwd().openFile(program_path, .{});
            defer file.close();

            const header = try std.elf.Header.read(file);

            var program_header_iterator = header.program_header_iterator(file);
            while (try program_header_iterator.next()) |ph| {
                const buffer = try allocator.alloc(u8, ph.p_filesz);
                defer allocator.free(buffer);

                _ = try file.preadAll(buffer, ph.p_offset);
                cpu.data.mmio_write(ph.p_vaddr, buffer);
            }

            cpu.harts[0].data.I.pc = @truncate(header.entry);

            var string_table: ?[]u8 = null;

            var section_header_iterator = header.section_header_iterator(file);
            var i: usize = 0;
            while (try section_header_iterator.next()) |sh| {
                if (i != header.shstrndx) {
                    i += 1;
                    continue;
                }
                const buffer = try allocator.alloc(u8, sh.sh_size);
                errdefer allocator.free(buffer);

                _ = try file.preadAll(buffer, sh.sh_offset);

                string_table = buffer;
                break;
            }

            if (string_table == null) {
                @panic("Cannot find string table");
            }

            var tohost_addr: ?u64 = null;

            section_header_iterator = header.section_header_iterator(file);
            while (try section_header_iterator.next()) |sh| {
                const span = std.mem.span(@as([*:0]u8, @ptrCast(&string_table.?[sh.sh_name])));
                if (std.mem.eql(u8, span, ".tohost")) {
                    tohost_addr = sh.sh_addr;
                }
            }

            if (tohost_addr == null) {
                @panic("Cannot find .tohost");
            }

            const io_tohost = try allocator.create(IOTOHOST);
            errdefer allocator.destroy(io_tohost);
            io_tohost.result = 0;

            try cpu.data.mmio_add(IOTOHOST, tohost_addr.?, io_tohost);
            std.mem.reverse(base.BusEntry, cpu.data.bus.items);

            cpu.harts[0].data.Zicsr.mode = .M;

            return .{
                .allocator = allocator,
                .io_memory = io_memory,
                .io_tohost = io_tohost,
                .memory = std.mem.zeroes([8]u8),
                .cpu = cpu,
            };
        }

        pub fn step(self: *@This()) !void {
            errdefer self.allocator.destroy(self.cpu);
            errdefer self.cpu.deinit();
            errdefer self.allocator.destroy(self.io_tohost);
            errdefer self.allocator.free(self.io_memory.slice);
            errdefer self.allocator.destroy(self.io_memory);

            var old_values = std.mem.zeroes([4]ARCH.uarch());
            std.debug.assert(self.cpu.harts[0].data.read(&self.cpu.data, self.cpu.harts[0].data.I.pc, &self.memory));
            const instr = try ASM.from_memory(&self.memory);
            print("{s}: 0x{x} ", .{ self.cpu.harts[0].data.Zicsr.mode.name(), self.cpu.harts[0].data.I.pc });
            try instr.write(std.io.getStdErr().writer().any());
            for (0..instr.used_grs().len) |i| {
                old_values[i] = self.cpu.harts[0].data.I.regs[instr.used_grs()[i].to_u5()];
            }
            var old_memory: [8]u8 = undefined;
            @memcpy(&old_memory, &self.memory);
            const len = try instr.to_memory(&self.memory);
            if (!std.mem.eql(u8, self.memory[0..instr.len()], old_memory[0..len])) {
                std.mem.reverse(u8, self.memory[0..instr.len()]);
                std.mem.reverse(u8, old_memory[0..len]);
                print("Before: {b:0>8}\n", .{self.memory[0..instr.len()]});
                print("After: {b:0>8}\n", .{old_memory[0..len]});
                return error.LossyDissasambler;
            }

            self.cpu.harts[0].step(&self.cpu.data);

            for (0..instr.used_grs().len) |i| {
                const reg = instr.used_grs()[i];
                if (reg.to_u5() == 0) continue;
                print("\tReg: {s} = 0x{x} = 0x{x}\n", .{ base.IntRegNames[reg.to_u5()], old_values[i], self.cpu.harts[0].data.I.regs[reg.to_u5()] });
            }

            if (self.io_tohost.result & 1 != 1) {
                return;
            }
            const result = (self.io_tohost.result >> 1) & 0xffffffff;
            print("{}\n", .{result});
            if (result != 0) {
                std.process.exit(1);
            } else {
                std.process.exit(0);
            }
        }
    };
}
