const std = @import("std");
const print = std.debug.print;

const riscv = @import("riscv/cpu.zig");
const riscv_asm = @import("riscv/asm.zig");

const CPU = riscv.buildCPU(.X64, 1);
const elf = @import("elf.zig").build(CPU);

const IOMemory = @import("io/memory.zig");

const IOUART = struct {
    buffer: [8]u8 = std.mem.zeroes([8]u8),

    pub fn size(self: *@This()) u64 {
        return self.buffer.len;
    }

    pub fn read(self: *@This(), index: u64, buffer: []u8) void {
        @memcpy(buffer, self.buffer[index .. index + buffer.len]);
        buffer[5] ^= buffer[5] & 1;
    }

    pub fn write(self: *@This(), index: u64, buffer: []const u8) void {
        @memcpy(self.buffer[index .. index + buffer.len], buffer);
        std.debug.print("C: `{c}`\n", .{self.buffer[0]});
    }
};

pub fn main() !void {
    print("Running main\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var cpu = try CPU.init(gpa.allocator());
    defer cpu.deinit();

    // UART

    var io_uart = IOUART{};
    try cpu.add_mmio(IOUART, 0x10000000, &io_uart);

    // KERNEL

    var object = try elf.load(&cpu, "xv6/kernel/kernel");
    defer object.deinit();

    std.debug.print("Bus Entrys:\n", .{});
    for (cpu.bus.items) |entry| {
        std.debug.print("\t0x{x}-0x{x}\n", .{ entry.start, entry.end });
    }

    var uart = std.ArrayList(u8).init(gpa.allocator());
    defer uart.deinit();

    var memory: [8]u8 = undefined;
    var test_memory: [8]u8 = undefined;
    const ASM = @import("riscv/asm.zig").build_asm(.X64);
    var instr: ASM = undefined;
    var old_values = std.mem.zeroes([4]u64);
    while (d: {
        _ = try cpu.vmemory_read(cpu.harts[0].pc, &memory);
        instr = try ASM.from_memory(&memory);
        print("{s} 0x{x} ", .{ cpu.harts[0].mode.name(), cpu.harts[0].pc });
        try instr.write(std.io.getStdErr().writer().any());
        for (0..instr.used_grs().len) |i| {
            old_values[i] = cpu.harts[0].g_regs[instr.used_grs()[i].to_u5()];
        }
        const len = try instr.to_memory(&test_memory);
        if (!std.mem.eql(u8, memory[0..instr.len()], test_memory[0..len])) {
            std.mem.reverse(u8, memory[0..instr.len()]);
            std.mem.reverse(u8, test_memory[0..len]);
            print("Before: {b:0>8}\n", .{memory[0..instr.len()]});
            print("After: {b:0>8}\n", .{test_memory[0..len]});
            return error.LossyDissasambler;
        }
        break :d cpu.harts[0].step(&cpu);
    }) {
        for (0..instr.used_grs().len) |i| {
            const reg = instr.used_grs()[i];
            if (reg.to_u5() == 0) continue;
            print("\tReg: {s} = 0x{x} = 0x{x}\n", .{ riscv.IntRegNames[reg.to_u5()], old_values[i], cpu.harts[0].g_regs[reg.to_u5()] });
        }
    } else |err| {
        return err;
    }
}

const RHR = u8;
const THR = u8;
const IER = packed struct {
    RHRI: u1,
    THRI: u1,
    RLSI: u1,
    MSI: u1,
    _zero0: u4,
};

const FCR = packed struct {
    FIFO: u1,
    RFIFOR: u1,
    TFIFOR: u1,
    DMAMODES: u1,
    _zero1: u2,
    RCVR_T_LSB: u1,
    RCVR_T_MSB: u1,
};

const ISR = packed struct { IS: u1, IPB0: u1, IPB1: u1, IPB2: u1, _zero: u2, FIFO1: u1, FIFO2: u1 };
const LCR = packed struct { word_length: u2, STOP: u1, parity_enable: u1, even_parity: u1, set_parity: u1, set_break: u1, divisor_latch_enable: u1 };

const LSR = packed struct {
    RDRL: u1,
    OE: u1,
    PE: u1,
    FE: u1,
    BI: u1,
    THE: u1,
    TE: u1,
    FIFOE: u1,
};

const UART = packed struct { rhr: RHR, ier: IER, isr: ISR, lsr: LSR };

pub fn dump_hex(pad: []const u8, bytes: []const u8) void {
    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();
    dump_hex_fallible(pad, bytes) catch {};
}

pub fn dump_hex_fallible(pad: []const u8, bytes: []const u8) !void {
    const stderr = std.io.getStdErr();
    const writer = stderr.writer();
    var chunks = std.mem.window(u8, bytes, 16, 16);
    while (chunks.next()) |window| {
        try writer.writeAll(pad);

        // 2. Print the bytes.
        for (window, 0..) |byte, index| {
            try writer.print("{X:0>2} ", .{byte});
            if (index == 7) try writer.writeByte(' ');
        }
        try writer.writeByte(' ');
        if (window.len < 16) {
            var missing_columns = (16 - window.len) * 3;
            if (window.len < 8) missing_columns += 1;
            try writer.writeByteNTimes(' ', missing_columns);
        }

        // 3. Print the characters.
        try writer.writeByte('\n');
    }
}
