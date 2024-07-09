const std = @import("std");
const path = std.fs.path;

pub fn load(comptime CPU: type, cpu: *CPU, filename: []const u8) !void {
    var file = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer file.close();

    const header = std.elf.Header.read(file) catch {
        return;
    };

    var program_header_iterator = header.program_header_iterator(file);
    while (try program_header_iterator.next()) |prog| {
        if (prog.p_type == std.elf.PT_LOAD) {
            const p = cpu.get_memory_size();
            try cpu.set_memory_size(p + prog.p_memsz);
            try cpu.add_memory_map(.{ .start = p, .end = p + prog.p_memsz - 1, .to_start = prog.p_vaddr, .to_end = prog.p_vaddr + prog.p_memsz - 1 });
            var buffer = try std.ArrayList(u8).initCapacity(cpu.allocator, prog.p_memsz);
            defer buffer.deinit();
            for (0..prog.p_memsz) |_| {
                try buffer.append(0);
            }
            try file.seekTo(prog.p_offset);
            _ = try file.readAll(buffer.items);
            try cpu.vmemory_write_all(prog.p_vaddr, buffer.items);
        }
    }

    for (&cpu.harts) |*hart| {
        hart.pc = @truncate(header.entry);
    }
}
