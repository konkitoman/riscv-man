const std = @import("std");
const riscv = @import("riscv/cpu.zig");
const riscv_asm = @import("riscv/asm.zig");
const elf = @import("elf.zig");
const Allocator = std.mem.Allocator;

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

pub fn build(comptime arch: riscv.Arch) type {
    const ASM = riscv_asm.build_asm(.X64);
    const CPU = riscv.buildCPU(arch, 1);
    const ELF = elf.build(CPU);
    return struct {
        allocator: Allocator,
        io_tohost: *IOTOHOST,
        object: ELF,
        memory: [8]u8,
        cpu: *CPU,

        pub fn init(allocator: Allocator, program_path: []const u8) !@This() {
            const cpu = try allocator.create(CPU);
            errdefer allocator.destroy(cpu);
            cpu.* = try CPU.init(allocator);
            errdefer cpu.deinit();

            var object = ELF.load(cpu, program_path) catch |err| {
                print("Fail to load program: {s} Error: {}\n", .{ program_path, err });
                return err;
            };
            errdefer object.deinit();

            const tohost_addr = object.sections.get(".tohost").?;

            const io_tohost = try allocator.create(IOTOHOST);
            errdefer allocator.destroy(io_tohost);
            io_tohost.result = 0;

            try cpu.add_mmio(IOTOHOST, tohost_addr, io_tohost);
            std.mem.reverse(riscv.BusEntry, cpu.bus.items);

            return .{
                .allocator = allocator,
                .io_tohost = io_tohost,
                .object = object,
                .memory = std.mem.zeroes([8]u8),
                .cpu = cpu,
            };
        }

        pub fn step(self: *@This()) !void {
            errdefer self.allocator.destroy(self.cpu);
            errdefer self.cpu.deinit();
            errdefer self.object.deinit();
            errdefer self.allocator.destroy(self.io_tohost);

            var old_values = std.mem.zeroes([4]arch.uarch());
            _ = try self.cpu.vmemory_read(self.cpu.harts[0].pc, &self.memory);
            const instr = try ASM.from_memory(&self.memory);
            print("{s}: 0x{x} ", .{ self.cpu.harts[0].mode.name(), self.cpu.harts[0].pc });
            try instr.write(std.io.getStdErr().writer().any());
            for (0..instr.used_grs().len) |i| {
                old_values[i] = self.cpu.harts[0].g_regs[instr.used_grs()[i].to_u5()];
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

            try self.cpu.harts[0].step(self.cpu);

            for (0..instr.used_grs().len) |i| {
                const reg = instr.used_grs()[i];
                if (reg.to_u5() == 0) continue;
                print("\tReg: {s} = 0x{x} = 0x{x}\n", .{ riscv.IntRegNames[reg.to_u5()], old_values[i], self.cpu.harts[0].g_regs[reg.to_u5()] });
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
