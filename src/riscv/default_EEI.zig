const std = @import("std");
const base = @import("base.zig");
const debug = std.debug;
const Allocator = std.mem.Allocator;

pub const DataEEI = struct {
    allocator: Allocator,
    bus: std.ArrayListUnmanaged(base.BusEntry),

    pub fn mmio_add(self: *@This(), comptime TYPE: type, start: u64, io: *TYPE) !void {
        comptime {
            const size_fn = @typeInfo(@TypeOf(TYPE.size)).@"fn";
            const read_fn = @typeInfo(@TypeOf(TYPE.read)).@"fn";
            const write_fn = @typeInfo(@TypeOf(TYPE.write)).@"fn";
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
        try self.mmio_add_entry(.{
            .start = start,
            .end = start + TYPE.size(io) - 1,
            .io = io,
            .fn_write = @ptrCast(&TYPE.write),
            .fn_read = @ptrCast(&TYPE.read),
        });
    }

    pub fn mmio_add_entry(self: *@This(), new_entry: base.BusEntry) !void {
        if (new_entry.start > new_entry.end) {
            return error.StartIsBiggerThenEnd;
        }

        try self.bus.append(self.allocator, new_entry);
    }

    pub fn mmio_read(self: *@This(), address: u64, buffer: []u8) void {
        for (self.bus.items) |entry| {
            if (entry.start <= address and address + buffer.len <= entry.end) {
                entry.read(address - entry.start, buffer);
                return;
            }
        }

        debug.print("OUT of bound read: {x}-{x}", .{ address, address + buffer.len });
        @memset(buffer, 0);
    }

    pub fn mmio_write(self: *@This(), address: u64, buffer: []const u8) void {
        for (self.bus.items) |entry| {
            if (entry.start <= address and address <= entry.end) {
                entry.write(address - entry.start, buffer);
                return;
            }
        }

        debug.print("OUT of bound write: {x}-{x}", .{ address, address + buffer.len });
    }
};

pub fn buildEEI(comptime ARCH: base.Arch, comptime harts: usize, DataHart: type, comptime instructions: []const base.Instruction(ARCH, DataEEI, DataHart)) type {
    return struct {
        pub const Hart = struct {
            data: DataHart,

            pub fn step(self: *@This(), eei_data: *DataEEI) void {
                var instr_data: [4]u8 = undefined;

                if (!self.data.read(eei_data, self.data.I.pc, &instr_data)) return;

                var run_with: ?usize = null;

                for (0..instructions.len) |i| {
                    if (instructions[i].check(&instr_data)) {
                        if (run_with) |_| {
                            @panic("Instruction collision");
                        }
                        run_with = i;
                    }
                }

                if (run_with) |i| {
                    instructions[i].execute(eei_data, &self.data, &instr_data);
                } else {
                    @panic("Unknown instruction");
                }
            }
        };

        data: DataEEI,
        harts: [harts]Hart,

        const EEI = @This();

        pub fn init(allocator: Allocator, hart_data: DataHart) EEI {
            var self = EEI{
                .data = .{ .allocator = allocator, .bus = .{} },
                .harts = undefined,
            };

            for (&self.harts) |*hart| {
                hart.*.data = hart_data;
            }

            return self;
        }

        pub fn deinit(self: *EEI) void {
            self.data.bus.deinit(self.data.allocator);
        }
    };
}
