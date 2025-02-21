const std = @import("std");
const path = std.fs.path;

pub fn build(comptime CPU: type) type {
    return struct {
        cpu: *CPU,
        sections: std.StringHashMapUnmanaged(u64),

        pub fn load(cpu: *CPU, filename: []const u8) !@This() {
            var file = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
            defer file.close();

            const header = try std.elf.Header.read(file);

            var section_header_iterator = header.section_header_iterator(file);
            var section_headers = std.ArrayListUnmanaged(std.elf.Elf64_Shdr){};
            defer section_headers.deinit(cpu.allocator);
            while (try section_header_iterator.next()) |section| {
                try section_headers.append(cpu.allocator, section);
            }

            const string_section: []u8 = try cpu.allocator.alloc(u8, section_headers.items[header.shstrndx].sh_size);
            defer cpu.allocator.free(string_section);
            _ = try file.preadAll(string_section, section_headers.items[header.shstrndx].sh_offset);

            var sections = std.StringHashMapUnmanaged(u64){};

            std.debug.print("Sections:\n", .{});
            for (section_headers.items) |section_header| {
                const name_cstr: [*:0]const u8 = @ptrFromInt(@intFromPtr(string_section.ptr) + section_header.sh_name);
                std.debug.print("\t{s}: 0x{x}-0x{x}\n", .{ name_cstr, section_header.sh_addr, section_header.sh_addr + section_header.sh_size });
                const span = std.mem.span(name_cstr);
                const name = try cpu.allocator.alloc(u8, span.len);
                @memcpy(name, span);
                try sections.put(cpu.allocator, name, section_header.sh_addr);
            }

            var program_header_iterator = header.program_header_iterator(file);
            std.debug.print("Sections:\n", .{});
            while (try program_header_iterator.next()) |prog| {
                if (prog.p_type == std.elf.PT_LOAD) {
                    const buffer = try cpu.allocator.alloc(u8, prog.p_memsz);
                    defer cpu.allocator.free(buffer);

                    _ = try file.preadAll(buffer, prog.p_offset);

                    std.debug.print("\t0x{x}: 0x{x}..0x{x}\n", .{ prog.p_align, prog.p_vaddr, prog.p_vaddr + prog.p_memsz });

                    try cpu.mmio_write(prog.p_vaddr, buffer);
                }
            }

            for (&cpu.harts) |*hart| {
                hart.pc = @truncate(header.entry);
            }

            return .{
                .cpu = cpu,
                .sections = sections,
            };
        }

        pub fn deinit(self: *@This()) void {
            var key_iterator = self.sections.keyIterator();
            while (key_iterator.next()) |key| {
                self.cpu.allocator.free(key.*);
            }
            self.sections.deinit(self.cpu.allocator);
        }
    };
}
