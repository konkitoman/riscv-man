const std = @import("std");
const print = std.debug.print;

const riscv = @import("riscv/cpu.zig");
const riscv_asm = @import("riscv/asm.zig");
const elf = @import("elf.zig");

const CPU = riscv.buildCPU(.X64, 1);

pub fn main() !void {
    print("Running main\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var cpu = CPU.init(gpa.allocator(), &.{}, .{});
    defer cpu.deinit();

    try elf.load(CPU, &cpu, "xv6/kernel/kernel");

    var memory: [8]u8 = undefined;
    var test_memory: [8]u8 = undefined;
    const ASM = @import("riscv/asm.zig").build_asm(.X64);
    var instr: ASM = undefined;
    while (d: {
        _ = try cpu.vmemory_read(cpu.harts[0].pc, &memory);
        instr = try ASM.from_memory(&memory);
        print("0x{x} ", .{cpu.harts[0].pc});
        try instr.write(std.io.getStdErr().writer().any());
        for (instr.used_grs()) |reg| {
            if (reg.to_u5() == 0) continue;
            print("\tOld Reg: {s} = 0x{x}\n", .{ riscv.IntRegNames[reg.to_u5()], cpu.harts[0].g_regs[reg.to_u5()] });
        }
        const len = try instr.to_memory(&test_memory);
        if (!std.mem.eql(u8, memory[0..instr.len()], test_memory[0..len])) {
            std.mem.reverse(u8, memory[0..instr.len()]);
            std.mem.reverse(u8, test_memory[0..len]);
            print("Before: {b:0>8}\n", .{memory[0..instr.len()]});
            print("After: {b:0>8}\n", .{test_memory[0..len]});
            return error.LossyDissasambler;
        }
        break :d cpu.step();
    }) {} else |err| {
        print("{}\n", .{err});
    }
}

pub fn dump_csrs(hart: CPU.Hart, csrs: []const riscv.CSRAddr) void {
    for (csrs) |csr| {
        print("{s} = ", .{@tagName(csr)});
        if (hart.csrs[csr.to_u12()].o_csr) |c| {
            print("{}\n", .{c});
        } else {
            print("Unimplemented\n", .{});
        }
    }
}

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
