const std = @import("std");
const base = @import("riscv/base.zig");
const default_EEI = @import("riscv/default_EEI.zig");
const riscv_asm = @import("riscv/asm.zig");
const elf = @import("elf.zig");
const Allocator = std.mem.Allocator;
const IOMemory = @import("io/memory.zig");

const I = @import("riscv/extension/I.zig");
const C = @import("riscv/extension/C.zig");
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
            defer inner.deinit();
            while (true) {
                try inner.step();
            }
        },
        64 => {
            var inner = try build(.X64).init(gpa.allocator(), program_path);
            defer inner.deinit();
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
    input: u64,
    output: u64,
    is_ready: bool,

    pub fn size(self: *@This()) u64 {
        _ = self;
        return 8 + 64 + 8;
    }

    pub fn read(self: *@This(), index: u64, buffer: []u8) void {
        if (index < 8) {
            @memcpy(buffer, std.mem.asBytes(&self.input)[index .. index + buffer.len]);
        } else if (index >= 0x40 and index < 0x40 + 8) {
            @memcpy(buffer, std.mem.asBytes(&self.output)[(index - 0x40) .. (index - 0x40) + buffer.len]);
        } else {
            print("Outof bounds: {}", .{index});
            @panic("");
        }
    }

    pub fn write(self: *@This(), index: u64, buffer: []const u8) void {
        if (index < 8) {
            if (buffer.len + index == 8) {
                self.is_ready = true;
            }
            @memcpy(std.mem.asBytes(&self.input)[index .. index + buffer.len], buffer);
        } else if (index >= 0x40 and index < 0x40 + 8) {
            @memcpy(std.mem.asBytes(&self.output)[(index - 0x40) .. (index - 0x40) + buffer.len], buffer);
        } else {
            print("Outof bounds: {}", .{index});
            @panic("");
        }
        // @memcpy(std.mem.asBytes(&self.input)[index .. index + buffer.len], buffer);
    }
};

pub fn build(comptime ARCH: Arch) type {
    const ASM = riscv_asm.build_asm(ARCH);
    return struct {
        const DataHart = struct {
            const uarch = ARCH.uarch();

            I: I.buildDataHart(ARCH),
            Zicsr: Zicsr.buildDataHart(ARCH),
            C: C.buildDataHart(ARCH),

            const CAUSE = Zicsr.buildDataHart(ARCH).CAUSE;

            pub fn va_to_pa_sv32(self: *@This(), eei_data: *DataEEI, info: u64, root: u32, va: u32, pa: *u32, i: u2) bool {
                if (i == 0) {
                    switch (info) {
                        1 => self.trap(CAUSE.LoadPageFault, va),
                        2 => self.trap(CAUSE.StorePageFault, va),
                        3 => self.trap(CAUSE.FetchPageFault, va),
                        else => {},
                    }
                    return false;
                }

                const pte_addr = root + (((va >> (2 + (@as(u5, 10) * i))) & std.math.maxInt(u10)) * 4);

                var page_buffer: [4]u8 = undefined;
                var pte: u32 = undefined;
                eei_data.mmio_read(pte_addr, &page_buffer);
                pte = std.mem.readInt(u32, &page_buffer, .little);
                if (pte & 1 == 0) {
                    switch (info) {
                        1 => self.trap(CAUSE.LoadPageFault, va),
                        2 => self.trap(CAUSE.StorePageFault, va),
                        3 => self.trap(CAUSE.FetchPageFault, va),
                        else => {},
                    }
                    return false;
                }

                if ((pte >> 1) & 0b111 == 0) {
                    return va_to_pa_sv32(self, eei_data, info, (pte >> 10) << 12, va, pa, i - 1);
                }

                //A leaf PTE has been reached. If i>0 and pte.ppn[i-1:0] ≠ 0, this is a misaligned superpage; stop
                //and raise a page-fault exception corresponding to the original access type.
                if (i == 2 and (pte >> 10) & 1 == 1) {
                    @panic("missaligned");
                }

                //Determine if the requested memory access is allowed by the pte.u bit, given the current privilege
                //mode and the value of the SUM and MXR fields of the mstatus register. If not, stop and raise a
                //page-fault exception corresponding to the original access type.
                switch (self.Zicsr.mode) {
                    .U => {
                        if ((pte >> 4) & 1 == 0) {
                            switch (info) {
                                1 => self.trap(CAUSE.LoadPageFault, va),
                                2 => self.trap(CAUSE.StorePageFault, va),
                                3 => self.trap(CAUSE.FetchPageFault, va),
                                else => {},
                            }
                            return false;
                        }
                    },
                    .S => {
                        const sstatus: Zicsr.X32SSTATUS = @bitCast(@as(u32, @truncate(self.Zicsr.sstatus)));
                        const mstatus: Zicsr.X32MSTATUS = @bitCast(@as(u32, @truncate(self.Zicsr.mstatus)));
                        if ((pte >> 4) & 1 == 1 and !(sstatus.SUM == 1 or mstatus.SUM == 1)) {
                            switch (info) {
                                1 => self.trap(CAUSE.LoadPageFault, va),
                                2 => self.trap(CAUSE.StorePageFault, va),
                                3 => self.trap(CAUSE.FetchPageFault, va),
                                else => {},
                            }
                            return false;
                        }
                    },
                    .M => {
                        const sstatus: Zicsr.X32SSTATUS = @bitCast(@as(u32, @truncate(self.Zicsr.sstatus)));
                        const mstatus: Zicsr.X32MSTATUS = @bitCast(@as(u32, @truncate(self.Zicsr.mstatus)));
                        if (mstatus.MPRV == 1) {
                            if ((pte >> 4) & 1 == 1 and !(sstatus.SUM == 1 or mstatus.SUM == 1)) {
                                switch (info) {
                                    1 => self.trap(CAUSE.LoadPageFault, va),
                                    2 => self.trap(CAUSE.StorePageFault, va),
                                    3 => self.trap(CAUSE.FetchPageFault, va),
                                    else => {},
                                }
                                return false;
                            }
                        }
                    },
                    else => {},
                }

                //Determine if the requested memory access is allowed by the pte.r, pte.w, and pte.x bits, given the
                //Shadow Stack Memory Protection rules. If not, stop and raise an access-fault exception.
                //Determine if the requested memory access is allowed by the pte.r, pte.w, and pte.x bits. If not, stop
                //and raise a page-fault exception corresponding to the original access type.
                switch (info) {
                    1 => {
                        if ((pte >> 1) & 1 == 0) {
                            self.trap(CAUSE.LoadPageFault, va);
                            return false;
                        }
                    },
                    2 => {
                        if ((pte >> 2) & 1 == 0) {
                            self.trap(CAUSE.StorePageFault, va);
                            return false;
                        }
                    },
                    3 => {
                        const sstatus: Zicsr.X32SSTATUS = @bitCast(@as(u32, @truncate(self.Zicsr.sstatus)));
                        if ((pte >> 3) & 1 == 0 and sstatus.MXR == 0) {
                            self.trap(CAUSE.FetchPageFault, va);
                            return false;
                        }
                    },
                    else => {},
                }

                const PTE_A = (pte >> 6) & 1;
                const PTE_D = (pte >> 7) & 1;
                if ((PTE_A == 0 or info == 2) // 2 = store
                and PTE_D == 0) {
                    //If a store to pte would violate a PMA or PMP check, raise an access-fault exception
                    //corresponding to the original access type.
                    //TODO

                    pte |= 1 << 6;
                    if (info == 2) {
                        pte |= 1 << 7;
                    }

                    std.mem.writeInt(u32, &page_buffer, pte, .little);
                    eei_data.mmio_write(pte_addr, &page_buffer);
                }

                if (i == 2) {
                    const new_addr: u32 = (((pte >> 10) & (std.math.maxInt(u12) << 10)) << 12) | (va & std.math.maxInt(u22));
                    pa.* = new_addr;
                } else {
                    const new_addr: u32 = ((pte >> 10) << 12) | (va & std.math.maxInt(u12));
                    pa.* = new_addr;
                }
                return true;
            }

            pub fn va_to_pa(self: *@This(), eei_data: *DataEEI, info: u64, va: uarch, pa: *uarch) bool {
                switch (self.Zicsr.xlen) {
                    .X32 => {
                        const satp = @as(Zicsr.X32SATP, @bitCast(@as(u32, @truncate(self.Zicsr.satp))));
                        switch (self.Zicsr.mode) {
                            .M => {
                                const mstatus = @as(Zicsr.X32MSTATUS, @bitCast(@as(u32, @truncate(self.Zicsr.mstatus))));
                                if (mstatus.MPRV == 1 and mstatus.MPP != 3 and info != 3) {
                                    if (satp.MODE == 0) {
                                        pa.* = va;
                                        return true;
                                    } else if (satp.MODE == 1) { // Sv32
                                        var _pa: u32 = 0;
                                        if (!self.va_to_pa_sv32(eei_data, info, @as(u32, satp.PPN) << 12, @truncate(va), &_pa, 2)) {
                                            return false;
                                        }

                                        pa.* = _pa;
                                        return true;
                                    }
                                }
                                pa.* = va;
                                return true;
                            },
                            .S => {
                                if (satp.MODE == 0) {
                                    pa.* = va;
                                    return true;
                                } else if (satp.MODE == 1) { // Sv32
                                    var _pa: u32 = 0;
                                    if (!self.va_to_pa_sv32(eei_data, info, @as(u32, satp.PPN) << 12, @truncate(va), &_pa, 2)) {
                                        return false;
                                    }

                                    pa.* = _pa;
                                    return true;
                                }
                            },
                            .U => {
                                if (satp.MODE == 0) {
                                    pa.* = va;
                                    return true;
                                } else if (satp.MODE == 1) { // Sv32
                                    var _pa: u32 = 0;
                                    if (!self.va_to_pa_sv32(eei_data, info, @as(u32, satp.PPN) << 12, @truncate(va), &_pa, 2)) {
                                        return false;
                                    }

                                    pa.* = _pa;
                                    return true;
                                }
                            },
                            else => std.debug.panic("Uimplemented mode {}\n", .{self.Zicsr.mode}),
                        }
                    },
                    .X64 => {
                        if (ARCH == .X32) unreachable;
                        const satp = @as(Zicsr.X64SATP, @bitCast(@as(u64, @truncate(self.Zicsr.satp))));
                        switch (self.Zicsr.mode) {
                            .M => {
                                pa.* = va;
                                return true;
                            },
                            .S => {
                                if (satp.MODE == 0) {
                                    pa.* = va;
                                    return true;
                                }
                                std.debug.panic("Unimplemented SATP MODE: {}\n", .{satp.MODE});
                            },
                            .U => {
                                if (satp.MODE == 0) {
                                    pa.* = va;
                                    return true;
                                }
                                std.debug.panic("Unimplemented SATP MODE: {}\n", .{satp.MODE});
                            },
                            else => std.debug.panic("Uimplemented mode {}\n", .{self.Zicsr.mode}),
                        }
                    },
                }

                return false;
            }

            pub fn read(self: *@This(), eei_data: *DataEEI, index: u64, buffer: []u8) bool {
                var pa: uarch = 0;
                if (!self.va_to_pa(eei_data, 1, @truncate(index), &pa)) return false;

                eei_data.mmio_read(pa, buffer);

                return true;
            }

            pub fn write(self: *@This(), eei_data: *DataEEI, index: u64, buffer: []const u8) bool {
                var pa: uarch = 0;
                if (!self.va_to_pa(eei_data, 2, @truncate(index), &pa)) return false;

                eei_data.mmio_write(pa, buffer);

                return true;
            }

            pub fn fence(self: *@This(), eei_data: *DataEEI) void {
                _ = eei_data;

                std.debug.print("FENCE not implemented\n", .{});
                self.I.pc += 4;
            }

            pub fn ecall(self: *@This(), eei_data: *DataEEI) void {
                _ = eei_data;

                switch (self.Zicsr.mode) {
                    .U => {
                        self.trap(CAUSE.ECallFromU, 0);
                    },
                    .S => {
                        self.trap(CAUSE.ECallFromS, 0);
                    },
                    .M => {
                        self.trap(CAUSE.ECallFromM, 0);
                    },
                    else => @panic("ECALL not implemented for virtual mode"),
                }
            }

            pub fn ebreak(self: *@This(), eei_data: *DataEEI) void {
                _ = eei_data;

                self.trap(CAUSE.Breakpoint, 0);
            }

            pub fn illegal_instruction(self: *@This()) void {
                std.debug.print("Illegal Instruction\n", .{});
                self.trap(CAUSE.IllegalInstruction, 0);
            }

            fn trap(self: *@This(), cause: CAUSE, address: uarch) void {
                if (cause.interrupt == 1) @panic("Interrupt not implemented");

                switch (self.Zicsr.mode) {
                    .U, .S => {
                        if ((self.Zicsr.medeleg >> @truncate(cause.code)) & 1 == 1) {
                            self.s_trap(cause, address);
                        } else {
                            self.m_trap(cause, address);
                        }
                    },
                    .M => {
                        self.m_trap(cause, address);
                    },
                    .H => {
                        @panic("Not implemented");
                    },
                }
            }

            pub fn m_trap(self: *@This(), cause: CAUSE, address: uarch) void {
                self.Zicsr.xlen = ARCH;

                switch (ARCH) {
                    .X32 => {
                        const mstatus = @as(*Zicsr.X32MSTATUS, @ptrCast(&self.Zicsr.mstatus));
                        mstatus.MPP = self.Zicsr.mode.to_u2();
                        self.Zicsr.mode = .M;
                        self.Zicsr.mepc = self.I.pc;
                        self.Zicsr.mcause = cause;
                        self.Zicsr.mtval = address;
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
                        self.Zicsr.mtval = address;
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

            fn s_trap(self: *@This(), cause: CAUSE, address: uarch) void {
                self.Zicsr.xlen = ARCH;

                switch (ARCH) {
                    .X32 => {
                        const sstatus = @as(*Zicsr.X32SSTATUS, @ptrCast(&self.Zicsr.sstatus));
                        sstatus.SPP = @truncate(self.Zicsr.mode.to_u2());
                        self.Zicsr.mode = .S;
                        self.Zicsr.sepc = self.I.pc;
                        self.Zicsr.scause = cause;
                        self.Zicsr.stval = address;
                        const tvec = @as(Zicsr.X32TVEC, @bitCast(self.Zicsr.stvec));
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
                        const sstatus = @as(*Zicsr.X64SSTATUS, @ptrCast(&self.Zicsr.sstatus));
                        sstatus.SPP = @truncate(self.Zicsr.mode.to_u2());
                        self.Zicsr.mode = .S;
                        self.Zicsr.sepc = self.I.pc;
                        self.Zicsr.scause = cause;
                        self.Zicsr.stval = address;
                        const tvec = @as(Zicsr.X64TVEC, @bitCast(self.Zicsr.stvec));
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
                                @panic("Unknown STVEC Mode!");
                            },
                        }
                    },
                }
            }
        };

        const INSTRS = I.buildInstrs(ARCH, DataEEI, DataHart) ++ Zifencei.buildInstrs(ARCH, DataEEI, DataHart) ++ Zicsr.buildInstrs(ARCH, DataEEI, DataHart) ++ C.buildInstrs(ARCH, DataEEI, DataHart);
        const EEI = default_EEI.buildEEI(ARCH, 1, DataHart, &INSTRS);
        const ELF = elf.build(EEI);

        allocator: Allocator,
        io_tohost: *IOTOHOST,
        tohost_buffer: std.ArrayListUnmanaged(u8) = .{},
        memory: [8]u8,
        io_memory: *IOMemory,
        cpu: *EEI,

        pub fn init(allocator: Allocator, program_path: []const u8) !@This() {
            const cpu = try allocator.create(EEI);
            errdefer allocator.destroy(cpu);
            cpu.* = EEI.init(allocator, std.mem.zeroInit(DataHart, .{}));
            errdefer cpu.deinit();

            const memory = try allocator.alloc(u8, 1024 * 1024 * 2); // 2MB
            @memset(memory, 0);
            errdefer allocator.free(memory);

            const io_memory = try allocator.create(IOMemory);
            errdefer allocator.destroy(io_memory);

            io_memory.* = IOMemory.init(memory);

            try cpu.data.mmio_add(IOMemory, 0x80000000, io_memory);

            // Loading ELF
            var file = try std.fs.cwd().openFile(program_path, .{});
            defer file.close();

            const header = try std.elf.Header.read(file);

            print("Sections:\n", .{});
            var program_header_iterator = header.program_header_iterator(file);
            while (try program_header_iterator.next()) |ph| {
                if (ph.p_type != std.elf.PT_LOAD) continue;
                print("\t{x}-{x}\n", .{ ph.p_vaddr, ph.p_vaddr + ph.p_memsz });

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

            defer allocator.free(string_table.?);

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
            io_tohost.input = 0;
            io_tohost.output = 0;

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

        pub fn deinit(self: *@This()) void {
            self.cpu.deinit();
            self.allocator.destroy(self.cpu);
            self.allocator.destroy(self.io_tohost);
            self.allocator.free(self.io_memory.slice);
            self.allocator.destroy(self.io_memory);
        }

        pub fn step(self: *@This()) !void {
            const va = self.cpu.harts[0].data.I.pc;
            print("{s}: {x} ", .{ self.cpu.harts[0].data.Zicsr.mode.name(), va });

            var pa: ARCH.uarch() = 0;
            if (!self.cpu.harts[0].data.va_to_pa(&self.cpu.data, 3, va, &pa)) {
                print("Cannot translate address\n", .{});
                return;
            }
            if (va != pa) {
                print("-> {x} ", .{pa});
            }

            var old_values = std.mem.zeroes([4]ARCH.uarch());
            self.cpu.data.mmio_read(pa, &self.memory);
            const instr = try ASM.from_memory(&self.memory);
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

            self.cpu.harts[0].step(&self.cpu.data, &self.memory);

            for (0..instr.used_grs().len) |i| {
                const reg = instr.used_grs()[i];
                if (reg.to_u5() == 0) continue;
                print("\tReg: {s} = 0x{x} = 0x{x}\n", .{ base.IntRegNames[reg.to_u5()], old_values[i], self.cpu.harts[0].data.I.regs[reg.to_u5()] });
            }

            // https://github.com/riscv-software-src/riscv-isa-sim/issues/364#issuecomment-607657754
            if (!self.io_tohost.is_ready) {
                return;
            }
            self.io_tohost.is_ready = false;

            const tohost_device = self.io_tohost.input >> 56;
            const tohost_command = (self.io_tohost.input >> 48) & 0b1111111;

            switch (tohost_device) {
                0 => { // syscall device
                    if (tohost_command != 0) {
                        print("invalid syscall device command: {}\n", .{tohost_command});
                        return;
                    }

                    const subfunction = self.io_tohost.input & 1;

                    if (subfunction == 1) { // exit
                        if (self.tohost_buffer.items.len != 0) {
                            print("TOHOST Output: {s}\n", .{self.tohost_buffer.items});
                        }

                        const result = (self.io_tohost.input >> 1) & std.math.maxInt(u31);
                        print("{}\n", .{result});
                        if (result != 0) {
                            std.process.exit(1);
                        } else {
                            std.process.exit(0);
                        }
                    } else { // syscall
                        const syscall_ptr = (self.io_tohost.input >> 1) & std.math.maxInt(u31);
                        if (syscall_ptr == 0) return;
                        print("syscall not implemented: {}\n", .{syscall_ptr});
                        return;
                    }
                },
                1 => { // blocking character device
                    switch (tohost_command) {
                        1 => { // write
                            const char: u8 = @truncate(self.io_tohost.input);
                            try self.tohost_buffer.append(self.allocator, char);
                            if (char == '\n') {
                                print("TOHOST Output: {s}", .{self.tohost_buffer.items});
                                self.tohost_buffer.clearRetainingCapacity();
                            }

                            self.io_tohost.input = 0;
                            self.io_tohost.output = 1;
                        },
                        else => {
                            print("TOHOST unknown blocking character device command: {}\n", .{tohost_command});
                            return;
                        },
                    }
                },
                else => {
                    print("Unknown TOHOST device: {} and command {}\n", .{ tohost_device, tohost_command });
                    print("{b:0>64}\n", .{self.io_tohost.input});

                    return;
                },
            }
        }
    };
}
