const std = @import("std");
const default_EEI = @import("riscv/default_EEI.zig");
const base = @import("riscv/base.zig");

const IOMemory = @import("io/memory.zig");

const I = @import("riscv/extension/I.zig");
const Zicsr = @import("riscv/extension/Zicsr.zig");

const Allocator = std.mem.Allocator;

const Arch = base.Arch;

const ARCH = Arch.X64;

const DataEEI = default_EEI.DataEEI;

const DataHart = struct {
    const uarch = ARCH.uarch();

    I: I.buildDataHart(ARCH),
    Zicsr: Zicsr.buildDataHart(ARCH),

    const CAUSE = Zicsr.buildDataHart(ARCH).CAUSE;

    pub fn read(self: *@This(), eei_data: *DataEEI, index: u64, buffer: []u8) bool {
        _ = self;

        eei_data.mmio_read(index, buffer);

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

        std.debug.print("ECALL not implemented\n", .{});
        self.I.pc += 4;
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
        self.Zicsr.mstatus.MPP = self.Zicsr.mode.to_u2();
        self.Zicsr.mode = .M;
        self.Zicsr.mepc = self.I.pc;
        self.Zicsr.mcause = cause;
        const tvec = self.Zicsr.mtvec;
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

const INSTRS = I.buildInstrs(ARCH, DataEEI, DataHart) ++ Zicsr.buildInstrs(ARCH, DataEEI, DataHart);

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const alloc = gpa.allocator();

    const CPU = default_EEI.buildEEI(ARCH, 1, DataHart, &INSTRS);

    var eei = CPU.init(alloc, std.mem.zeroes(DataHart));
    defer eei.deinit();

    const memory = try alloc.alloc(u8, 1024 * 1000 * 2); // MBs
    defer alloc.free(memory);

    var io_memory = IOMemory.init(memory);

    try eei.data.mmio_add(IOMemory, 0x80000000, &io_memory);

    var args = try std.process.argsWithAllocator(alloc);
    _ = args.next(); // current path
    const path = args.next().?;

    var file = try std.fs.cwd().openFile(path, .{ .mode = .read_only });
    defer file.close();

    const elf_header = try std.elf.Header.read(file);
    var program_header_iterator = elf_header.program_header_iterator(file);

    while (try program_header_iterator.next()) |ph| {
        if (ph.p_type != std.elf.PT_LOAD) continue;
        const buffer = try alloc.alloc(u8, ph.p_memsz);
        defer alloc.free(buffer);

        _ = try file.preadAll(buffer, ph.p_offset);
        eei.data.mmio_write(ph.p_vaddr, buffer);
    }

    eei.harts[0].data.I.pc = elf_header.entry;
    eei.harts[0].data.Zicsr.mode = .M;

    while (true) {
        eei.harts[0].step(&eei.data);
    }
}
