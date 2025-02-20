const std = @import("std");
const print = std.debug.print;

const riscv = @import("riscv/cpu.zig");
const riscv_asm = @import("riscv/asm.zig");

const CPU = riscv.buildCPU(.X64, 1);
const elf = @import("elf.zig").build(CPU);

const IOMemory = @import("io/memory.zig");

const IOUART = struct {
    output: std.ArrayList(u8),
    ier: IER = std.mem.zeroes(IER),
    fcr: FCR = std.mem.zeroes(FCR),
    lcr: LCR = std.mem.zeroes(LCR),

    pub fn size(self: *@This()) u64 {
        _ = self;
        return 8;
    }

    pub fn read(self: *@This(), index: u64, buffer: []u8) void {
        std.debug.assert(buffer.len == 1);

        _ = self;

        switch (index) {
            0 => {
                print("Request read\n", .{});
                buffer[0] = 0;
            },
            5 => {
                print("Request LSR\n", .{});
                buffer[0] |= 1;
            },
            else => {
                print("Request UNKNOWN Read {}\n", .{index});
                @panic("");
            },
        }
    }

    pub fn write(self: *@This(), index: u64, buffer: []const u8) void {
        std.debug.assert(buffer.len == 1);

        switch (index) {
            0 => {
                print("C: {X}\n", .{buffer[0]});
                self.output.append(buffer[0]) catch {
                    @panic("OUT OF MEMMORY");
                };
            },
            1 => {
                self.ier = @bitCast(buffer[0]);
                print("IER: {}\n", .{self.ier});
            },
            2 => {
                self.fcr = @bitCast(buffer[0]);
                print("FCR: {}\n", .{self.fcr});
            },
            3 => {
                self.lcr = @bitCast(buffer[0]);
                print("LCR: {}\n", .{self.lcr});
            },
            else => {
                print("Request UNKNOWN Write {}\n", .{index});
                @panic("");
            },
        }
    }
};

pub fn main() !void {
    print("Running main\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var cpu = try CPU.init(gpa.allocator());
    defer cpu.deinit();

    // UART

    var io_uart = IOUART{ .output = std.ArrayList(u8).init(gpa.allocator()) };
    defer io_uart.output.deinit();
    try cpu.add_mmio(IOUART, 0x10000000, &io_uart);

    // KERNEL

    var object = try elf.load(&cpu, "xv6/kernel/kernel");
    defer object.deinit();

    std.debug.print("Bus Entrys:\n", .{});
    for (cpu.bus.items) |entry| {
        std.debug.print("\t0x{x}-0x{x}\n", .{ entry.start, entry.end });
    }

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
        print("UART: `{s}`\n", .{io_uart.output.items});
    } else |err| {
        return err;
    }
}

const RHR = u8;
const THR = u8;
const IER = packed struct {
    receive_holding_RI: u1,
    transmit_holding_RI: u1,
    receive_line_SI: u1,
    modem_SI: u1,
    _zero0: u4,
};
const FCR = packed struct {
    FIFO_enabled: u1,
    reciver_FIFO_reset: u1,
    transmit_FIFO_reset: u1,
    DMA_mode_select: u1,
    _zero0: u2,
    RCVR_trigger_LSB: u1,
    RCVR_trigger_MSB: u1,
};
const ISR = packed struct {
    IS: u1,
    IP: u3,
    _zero: u2,
    FIFO1: u1,
    FIFO2: u1,
};
const LCR = packed struct {
    word_length: u2,
    STOP: u1,
    parity_enable: u1,
    even_parity: u1,
    set_parity: u1,
    set_break: u1,
    divisor_latch_enable: u1,
};
const MCR = packed struct {
    DTR: u1,
    RTS: u1,
    OP1: u1,
    OP2: u1,
    loop_back: u1,
    _zero: u3,
};
const LSR = packed struct {
    receive_data_ready: u1,
    overrun_error: u1,
    parity_error: u1,
    framing_error: u1,
    break_I: u1,
    transmit_holding_empty: u1,
    transmit_empty: u1,
    FIFO_error: u1,
};
const MSR = packed struct {
    delta_CTS: u1,
    delta_DSR: u1,
    delta_R1: u1,
    delta_CD: u1,
    CTS: u1,
    DSR: u1,
    RI: u1,
    CD: u1,
};
const SPR = u8;

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
