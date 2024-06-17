const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("riscv-base.zig");
pub const GeneralRegsNames = base.GeneralRegsNames;
pub const GeneralReg = base.GeneralReg;
pub const VarInstr = base.VarInstr;
pub const ImmediateVariant = base.ImmediateVariantX32;
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

pub const CPU = struct {
    pub const Thread = struct {
        g_regs: [32]u64,
        pc: usize,
        csrs: [4096]u64,

        pub fn step(self: *Thread, cpu: *CPU) !void {
            var memory: [8]u8 = undefined;
            const memory_len = try cpu.vmemory_read(self.pc, &memory);
            if (memory_len == 0) {
                return error.EndOfStream;
            }
            const e_var_instr = VarInstr.from_memory(memory[0..memory_len]);

            if (e_var_instr) |var_instr| switch (var_instr) {
                .x16 => |x16| {
                    print("Found x16 instr: {x}\n", .{x16});
                },
                .x32 => |x32| {
                    const instr = ImmediateVariant.from_u32(x32);

                    switch (instr.opcode) {
                        0b0110111 => { // LUI
                            const u = instr.u;
                            if (u.rd != 0) {
                                self.g_regs[u.rd] = 0;
                                self.g_regs[u.rd] = @as(u64, u.imm_31_12) << 12;
                            }
                            self.pc += 4;
                        },
                        0b0010111 => { // AUIPC
                            const offset = @as(u64, instr.u.imm_31_12) << 12;
                            if (instr.u.rd != 0) {
                                self.g_regs[instr.u.rd] = self.pc + offset;
                            }
                            self.pc += 4;
                        },
                        0b1101111 => { // JAL
                            if (instr.u.rd != 0) {
                                self.g_regs[instr.u.rd] = self.pc + 4;
                            }
                            self.pc = @as(u64, @bitCast(@as(i64, @bitCast(self.pc)) + (@as(i64, instr.j.get_imm()) * 2)));
                        },
                        0b1100111 => { // JALR
                            if (instr.i.rd != 0) {
                                self.g_regs[instr.i.rd] = self.pc + 4;
                            }
                            self.pc = @as(u64, @bitCast(@as(i64, @bitCast(self.g_regs[instr.i.rs1])) + (@as(i64, @as(i12, @bitCast(instr.i.imm_11_0)))))) & 0xfffffffffffffffe;
                        },
                        0b1100011 => { // BRANCH
                            const offset = @as(i64, instr.b.get_imm()) * 2;
                            switch (instr.b.funct3) {
                                0b000 => { // BEQ
                                    if (self.g_regs[instr.b.rs1] == self.g_regs[instr.b.rs2]) {
                                        self.pc = @as(u64, @bitCast(@as(i64, @bitCast(self.pc)) + offset));
                                        return;
                                    }
                                },
                                0b001 => { // BNE
                                    if (self.g_regs[instr.b.rs1] != self.g_regs[instr.b.rs2]) {
                                        self.pc = @as(u64, @bitCast(@as(i64, @bitCast(self.pc)) + offset));
                                        return;
                                    }
                                },
                                0b100 => { // BLT
                                    if (@as(i64, @bitCast(self.g_regs[instr.b.rs1])) < @as(i64, @bitCast(self.g_regs[instr.b.rs2]))) {
                                        self.pc = @as(u64, @bitCast(@as(i64, @bitCast(self.pc)) + offset));
                                        return;
                                    }
                                },
                                0b101 => { // BGE
                                    if (@as(i64, @bitCast(self.g_regs[instr.b.rs1])) > @as(i64, @bitCast(self.g_regs[instr.b.rs2]))) {
                                        self.pc = @as(u64, @bitCast(@as(i64, @bitCast(self.pc)) + offset));
                                        return;
                                    }
                                },
                                0b110 => { // BLTU
                                    if (self.g_regs[instr.b.rs1] < self.g_regs[instr.b.rs2]) {
                                        self.pc = @as(u64, @bitCast(@as(i64, @bitCast(self.pc)) + offset));
                                        return;
                                    }
                                },
                                0b111 => { // BGEU
                                    if (self.g_regs[instr.b.rs1] > self.g_regs[instr.b.rs2]) {
                                        self.pc = @as(u64, @bitCast(@as(i64, @bitCast(self.pc)) + offset));
                                        return;
                                    }
                                },
                                else => {
                                    print("Invalid BRANCH func3: {b:0>3}", .{instr.b.funct3});
                                },
                            }
                            self.pc += 4;
                        },
                        0b0000011 => { // LOAD
                            const offset: u64 = @bitCast(@as(i64, @bitCast(self.g_regs[instr.i.rs1])) + @as(i64, instr.i.imm_11_0));
                            switch (instr.i.funct3) {
                                0b000 => { // LB
                                    var buffer: [1]u8 = undefined;
                                    try cpu.vmemory_read_all(offset, &buffer);
                                    self.g_regs[instr.i.rd] = @bitCast(@as(i64, std.mem.readInt(i8, &buffer, .little)));
                                },
                                0b001 => { // LH
                                    var buffer: [2]u8 = undefined;
                                    try cpu.vmemory_read_all(offset, &buffer);
                                    self.g_regs[instr.i.rd] = @bitCast(@as(i64, std.mem.readInt(i16, &buffer, .little)));
                                },
                                0b010 => { // LW
                                    var buffer: [4]u8 = undefined;
                                    try cpu.vmemory_read_all(offset, &buffer);
                                    self.g_regs[instr.i.rd] = @bitCast(@as(i64, std.mem.readInt(i32, &buffer, .little)));
                                },
                                0b011 => { // LD
                                    var buffer: [8]u8 = undefined;
                                    try cpu.vmemory_read_all(offset, &buffer);
                                    self.g_regs[instr.i.rd] = @bitCast(std.mem.readInt(i64, &buffer, .little));
                                },
                                0b100 => { // LBU
                                    var buffer: [1]u8 = undefined;
                                    try cpu.vmemory_read_all(offset, &buffer);
                                    self.g_regs[instr.i.rd] = @bitCast(@as(u64, std.mem.readInt(u8, &buffer, .little)));
                                },
                                0b101 => { // LHU
                                    var buffer: [2]u8 = undefined;
                                    try cpu.vmemory_read_all(offset, &buffer);
                                    self.g_regs[instr.i.rd] = @bitCast(@as(u64, std.mem.readInt(u16, &buffer, .little)));
                                },
                                else => {
                                    print("Invalid LOAD func3: {b:0>3}\n", .{instr.b.funct3});
                                },
                            }
                            self.pc += 4;
                        },
                        0b0100011 => { // STORE
                            const offset: u64 = @bitCast((@as(i64, @bitCast(self.g_regs[instr.s.rs1])) + instr.s.get_imm()));
                            var buffer: [8]u8 = undefined;
                            std.mem.writeInt(u64, &buffer, self.g_regs[instr.s.rs2], .little);
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
                                    print("Store D: {x}, buffer: {x}\n", .{ offset, buffer.len });
                                    try cpu.vmemory_write_all(offset, buffer[0..8]);
                                },
                                else => {
                                    print("Invalid STORE func3: {b:0>3}\n", .{instr.b.funct3});
                                },
                            }
                            self.pc += 4;
                        },
                        0b0010011 => {
                            const i = instr.i;
                            switch (instr.i.funct3) {
                                0b000 => { // ADDI
                                    if (i.rd != 0) {
                                        self.g_regs[i.rd] = self.g_regs[i.rs1] +% @as(u64, @bitCast(@as(i64, i.imm_11_0)));
                                    }
                                },
                                0b010 => { // SLTI
                                    if (i.rd != 0) {
                                        self.g_regs[i.rd] = if (@as(i64, @bitCast(self.g_regs[i.rs1])) < @as(i64, i.imm_11_0)) 1 else 0;
                                    }
                                },
                                0b011 => { // SLTIU
                                    if (i.rd != 0) {
                                        self.g_regs[i.rd] = if (self.g_regs[i.rs1] == 0) 1 else if (self.g_regs[i.rs1] < @as(u64, @bitCast(@as(i64, i.imm_11_0)))) 1 else 0;
                                    }
                                },
                                0b100 => { // XORI
                                    if (i.rd != 0) {
                                        self.g_regs[i.rd] = @as(u64, @bitCast(@as(i64, @bitCast(self.g_regs[i.rs1])) ^ @as(i64, i.imm_11_0)));
                                    }
                                },
                                0b110 => { // ORI
                                    if (i.rd != 0) {
                                        self.g_regs[i.rd] = @as(u64, @bitCast(@as(i64, @bitCast(self.g_regs[i.rs1])) | @as(i64, i.imm_11_0)));
                                    }
                                },
                                0b111 => { // ANDI
                                    if (i.rd != 0) {
                                        self.g_regs[i.rd] = @as(u64, @bitCast(@as(i64, @bitCast(self.g_regs[i.rs1])) & @as(i64, i.imm_11_0)));
                                    }
                                },
                                0b001 => { // SLLI
                                    switch (instr.i_1.op) {
                                        0b000000 => {
                                            if (instr.i_1.rd != 0) {
                                                self.g_regs[instr.i_1.rd] = self.g_regs[instr.i_1.rs1] << instr.i_1.shamt;
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
                                                self.g_regs[i.rd] = self.g_regs[i.rs1] >> instr.i_1.shamt;
                                            }
                                        },
                                        0b010000 => { // SRAI
                                            if (i.rd != 0) {
                                                const mask = (@as(u64, std.math.maxInt(u64)) >> instr.i_1.shamt) ^ @as(u64, std.math.maxInt(u64));
                                                self.g_regs[i.rd] = (self.g_regs[i.rs1] >> instr.i_1.shamt) | (mask & if (self.g_regs[i.rs1] & 1 << 63 == 1 << 63) @as(u64, std.math.maxInt(u64)) else 0);
                                            }
                                        },
                                        else => {
                                            print("Special I, not implemented: {b:0>6}\n", .{instr.i_1.op});
                                        },
                                    }
                                },
                            }
                            self.pc += 4;
                        },
                        0b0110011 => { // OP-IMM
                            const r = instr.r;
                            switch (r.funct3) {
                                0b000 => { // ADD/SUB
                                    switch (r.funct7) {
                                        0b0000000 => { // ADD
                                            if (r.rd != 0) {
                                                self.g_regs[r.rd] = self.g_regs[r.rs1] +% self.g_regs[r.rs2];
                                            }
                                        },
                                        0b0100000 => { // SUB
                                            if (r.rd != 0) {
                                                self.g_regs[r.rd] = self.g_regs[r.rs1] -% self.g_regs[r.rs2];
                                            }
                                        },
                                        else => {
                                            print("Invalid funct7 for ADD or SUB {b:0>7}\n", .{r.funct7});
                                        },
                                    }
                                },
                                0b001 => { // SLL
                                    if (r.rd != 0) {
                                        self.g_regs[r.rd] = self.g_regs[r.rs1] << @as(u6, @truncate(self.g_regs[r.rs2]));
                                    }
                                },
                                0b010 => { // SLT
                                    if (r.rd != 0) {
                                        self.g_regs[r.rd] = if (@as(i64, @bitCast(self.g_regs[r.rs1])) < @as(i64, @bitCast(self.g_regs[r.rs2]))) 1 else 0;
                                    }
                                },
                                0b011 => { // SLTU
                                    if (r.rd != 0) {
                                        self.g_regs[r.rd] = if (self.g_regs[r.rs1] == 0) 1 else if (self.g_regs[r.rs1] < self.g_regs[r.rs2]) 1 else 0;
                                    }
                                },
                                0b100 => { // XOR
                                    if (r.rd != 0) {
                                        self.g_regs[r.rd] = self.g_regs[r.rs1] ^ self.g_regs[r.rs2];
                                    }
                                },
                                0b101 => { // SRL/SRA
                                    switch (r.funct7) {
                                        0b0000000 => { // SRL
                                            if (r.rd != 0) {
                                                self.g_regs[r.rd] = self.g_regs[r.rs1] >> @as(u6, @truncate(self.g_regs[r.rs2]));
                                            }
                                        },
                                        0b0100000 => { // SRA
                                            if (r.rd != 0) {
                                                const mask = (@as(u64, std.math.maxInt(u64)) >> @as(u6, @truncate(self.g_regs[r.rs2]))) ^ @as(u64, std.math.maxInt(u64));
                                                self.g_regs[r.rd] = (self.g_regs[r.rs1] >> @as(u6, @truncate(self.g_regs[r.rs2]))) | (mask & if (self.g_regs[r.rs1] & 1 << 63 == 1 << 63) @as(u64, std.math.maxInt(u64)) else 0);
                                            }
                                        },
                                        else => {
                                            print("Invalid funct7 for SRL or SRA {b:0>7}\n", .{r.funct7});
                                        },
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
                            self.pc += 4;
                        },
                        0b0001111 => { // MISC-MEM
                            switch (instr.f.func3) {
                                0b000 => {
                                    print("FENCE is doing nothing!\n", .{});
                                },
                                else => {
                                    print("Invalid func3 for MISC-MEM {b:0>3}\n", .{instr.f.func3});
                                },
                            }
                            self.pc += 4;
                        },
                        0b1110011 => { // SYSTEM
                            switch (instr.i.funct3) {
                                0b000 => switch (instr.i.imm_11_0) {
                                    0 => { // ECALL
                                        print("ECALL not implemented!\n", .{});
                                    },
                                    1 => { // EBREAK
                                        return error.Break;
                                    },
                                    else => {
                                        print("Invalid SYSTEM call {b:0>12}\n", .{instr.i.imm_11_0});
                                    },
                                },
                                // CSRRW
                                0b001 => {
                                    const csr: u12 = @bitCast(instr.i.imm_11_0);
                                    if (instr.i.rd != 0) {
                                        self.g_regs[instr.i.rd] = self.csrs[csr];
                                    }
                                    self.csrs[csr] = self.g_regs[instr.i.rs1];
                                },
                                // CSRRS
                                0b010 => {
                                    const csr: u12 = @bitCast(instr.i.imm_11_0);
                                    if (instr.i.rd == 0) {
                                        self.g_regs[instr.i.rd] = self.csrs[csr];
                                    }
                                    if (instr.i.rs1 != 0) {
                                        self.csrs[csr] |= self.g_regs[instr.i.rs1];
                                    }
                                },
                                // CSRRC
                                0b011 => {
                                    const csr: u12 = @bitCast(instr.i.imm_11_0);
                                    if (instr.i.rd == 0) {
                                        self.g_regs[instr.i.rd] = self.csrs[csr];
                                    }
                                    if (instr.i.rs1 != 0) {
                                        self.csrs[csr] ^= self.csrs[csr] & self.g_regs[instr.i.rs1];
                                    }
                                },
                                // CSRRWI
                                0b101 => {
                                    const csr: u12 = @bitCast(instr.i.imm_11_0);
                                    const value: u64 = @as(u5, @bitCast(instr.i.rs1));
                                    if (instr.i.rd != 0) {
                                        self.g_regs[instr.i.rd] = self.csrs[csr];
                                    }
                                    self.csrs[csr] = value;
                                },
                                // CSRRSI
                                0b110 => {
                                    const csr: u12 = @bitCast(instr.i.imm_11_0);
                                    const value: u64 = @as(u5, @bitCast(instr.i.rs1));
                                    if (instr.i.rd == 0) {
                                        self.g_regs[instr.i.rd] = self.csrs[csr];
                                    }
                                    if (value != 0) {
                                        self.csrs[csr] |= value;
                                    }
                                },
                                // CSRRCI
                                0b111 => {
                                    const csr: u12 = @bitCast(instr.i.imm_11_0);
                                    const value: u64 = @as(u5, @bitCast(instr.i.rs1));
                                    if (instr.i.rd == 0) {
                                        self.g_regs[instr.i.rd] = self.csrs[csr];
                                    }
                                    if (value != 0) {
                                        self.csrs[csr] ^= self.csrs[csr] & value;
                                    }
                                },
                                else => {
                                    print("Invalid SYSTEM funct3 {b:0>3}\n", .{instr.i.funct3});
                                },
                            }

                            self.pc += 4;
                        },
                        else => {
                            print("Opcode not implemented! {b:0>7}\n", .{instr.opcode});
                            self.pc += 4;
                        },
                    }
                },
                .x64 => |x64| {
                    print("Found x64 instr: {x}\n", .{x64});
                },
            } else |e| {
                print("When reading VarInstr at {x}, an error acured: {}\n", .{ self.pc, e });
            }
        }
    };

    threads: [1]Thread,
    allocator: Allocator,
    memory_maps: std.ArrayList(MemoryMap),
    memory: std.ArrayList(u8),

    pub fn init(allocator: Allocator) CPU {
        return .{
            .allocator = allocator,
            .threads = std.mem.zeroes([1]Thread),
            .memory_maps = std.ArrayList(MemoryMap).init(allocator),
            .memory = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(self: *CPU) void {
        self.memory_maps.deinit();
        self.memory.deinit();
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
            if (memory_map.end <= map.end and memory_map.start <= map.start) {
                return error.OverlappingMemory;
            }

            if (memory_map.to_end <= map.to_end and memory_map.to_start <= map.to_start) {
                return error.OverlappingVirtualMemory;
            }
        }

        try self.memory_maps.append(memory_map);
    }

    pub fn vmemory_read(self: CPU, address: u64, buffer: []u8) !u64 {
        var written: u64 = 0;
        for (0..buffer.len) |i| {
            const offset = self.map_to_memory(address + i) catch break;
            buffer[i] = self.memory.items[offset];
            written += 1;
        }
        return written;
    }

    pub fn vmemory_read_all(self: CPU, address: u64, buffer: []u8) !void {
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
        for (&self.threads) |*thread| {
            try thread.step(self);
        }
    }
};
