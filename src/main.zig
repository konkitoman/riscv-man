const std = @import("std");
const print = std.debug.print;

const riscv = @import("riscv.zig");
const ASM = @import("riscv-asm.zig");

pub fn main() !void {
    var args = std.process.args();
    const path = args.next();
    _ = path;

    const program_path = if (args.next()) |bin| bin else {
        print("program path is required!\n", .{});
        return;
    };

    print("Opening: `{s}`\n", .{program_path});

    const cwd = std.fs.cwd();

    const program_file = if (cwd.openFile(program_path[0..], .{ .mode = .read_only })) |f| f else |err| {
        if (err == error.FileNotFound) {
            print("File Not Found\n", .{});
        } else {
            print("Error when opeining file: {?}\n", .{err});
        }
        return;
    };

    print("Opened file: `{s}`\n", .{program_path});

    print("Trying to read as ELF format\n", .{});

    var elf_header_buf: [@sizeOf(std.elf.Elf64_Ehdr)]u8 align(@alignOf(std.elf.Elf64_Ehdr)) = undefined;
    try program_file.seekableStream().seekTo(0);
    try program_file.reader().readNoEof(&elf_header_buf);
    const elf_header = try std.elf.Header.parse(&elf_header_buf);
    const elf_header_raw = @as(*const std.elf.Elf64_Ehdr, @ptrCast(&elf_header_buf));

    var err = false;

    if (elf_header.machine != .RISCV) {
        print("Cannot open program made for machine: {?}\n", .{elf_header.machine});
        err = true;
    }

    if (elf_header.endian != .little) {
        print("Cannot read non little endian format!\n", .{});
        err = true;
    }

    if (err) {
        return;
    }
    err = false;

    print("Header: {?}\n", .{elf_header_raw});
    switch (elf_header_raw.e_type) {
        .NONE => {
            print("Type: None\n", .{});
        },
        .REL => {
            print("Type: Rel\n", .{});
        },
        .EXEC => {
            print("Type: Exec\n", .{});
        },
        .DYN => {
            print("Type: Dyn\n", .{});
        },
        .CORE => {
            print("Type: Core\n", .{});
        },
        // else => |t| {
        //     print("Type Unknown: 0x{x}\n", .{t});
        // },
    }

    var heap = std.heap.GeneralPurposeAllocator(.{}){};
    defer print("GA Status: {?}\n", .{heap.deinit()});
    var sections = try std.ArrayList(struct { section: std.elf.Elf64_Shdr, data: std.ArrayList(u8) }).initCapacity(heap.allocator(), elf_header.shnum);
    defer {
        for (sections.items) |sec| {
            sec.data.deinit();
        }
        sections.deinit();
    }
    var program_headers = try std.ArrayList(std.elf.Elf64_Phdr).initCapacity(heap.allocator(), elf_header.phnum);
    defer program_headers.deinit();

    {
        var section_iterator = elf_header.section_header_iterator(program_file);
        while (try section_iterator.next()) |section| {
            try sections.append(.{ .section = section, .data = try std.ArrayList(u8).initCapacity(heap.allocator(), @as(usize, section.sh_size)) });
        }
        var program_iterator = elf_header.program_header_iterator(program_file);
        while (try program_iterator.next()) |program_header| {
            try program_headers.append(program_header);
        }
    }

    for (sections.items) |*sec| {
        try program_file.seekTo(sec.section.sh_offset);
        sec.data.items.len = try program_file.reader().readAll(sec.data.allocatedSlice());
    }

    var program_section_index: usize = 0;
    var program_data_index: usize = 0;

    for (sections.items, 0..) |sec, i| {
        var name = sections.items[elf_header_raw.e_shstrndx].data.items[sec.section.sh_name..];
        for (name, 0..) |byte, b| {
            if (byte == 0) {
                name = name[0..b];
                break;
            }
        }

        print("{d} {s}, ", .{ i, name });
        switch (sec.section.sh_type) {
            std.elf.SHT_NULL => {
                print("Type: NULL, ", .{});
            },
            std.elf.SHT_PROGBITS => {
                print("Type: Program Data, ", .{});
                if (std.mem.eql(u8, name, ".text")) {
                    program_section_index = i;
                } else if (std.mem.eql(u8, name, ".data")) {
                    program_data_index = i;
                }
            },
            std.elf.SHT_SYMTAB => {
                print("Type: Symbol Table, ", .{});
            },
            std.elf.SHT_STRTAB => {
                print("Type: String Table, ", .{});
            },
            std.elf.SHT_RELA => {
                print("Type: Realocation ith Addens, ", .{});
            },
            std.elf.SHT_HASH => {
                print("Type: Symbol Hash Table, ", .{});
            },
            std.elf.SHT_DYNAMIC => {
                print("Type: Dynamic Linking, ", .{});
            },
            std.elf.SHT_REL => {
                print("Type: Realocation, ", .{});
            },
            std.elf.SHT_NOBITS => {
                print("Type: No Bits, ", .{});
            },
            std.elf.SHT_NOTE => {
                print("Type: Notes, ", .{});
            },
            std.elf.SHT_DYNSYM => {
                print("Type: Dynamic Linker Symbol Table, ", .{});
            },
            else => |ty| {
                print("Unknown Type: {d}, ", .{ty});
            },
        }
        print("Address: {x}\n", .{sec.section.sh_addr});
    }

    print("Program Section Index: {d}\n", .{program_section_index});

    for (program_headers.items) |program_header| {
        const type_name = switch (program_header.p_type) {
            std.elf.PT_NULL => "Null",
            std.elf.PT_LOAD => "Load",
            std.elf.PT_DYNAMIC => "Dynamic",
            std.elf.PT_INTERP => "Interp",
            std.elf.PT_NOTE => "Note",
            std.elf.PT_PHDR => "Phdr",
            std.elf.PT_TLS => "Thread local",
            std.elf.PT_NUM => "Num",
            std.elf.PT_LOOS => "LOOS",
            std.elf.PT_GNU_EH_FRAME => "GNU_EH_FRAME",
            std.elf.PT_GNU_STACK => "GNU_STACK",
            std.elf.PT_GNU_RELRO => "RELRO",
            else => "Unknown",
        };
        print("Program Header: {?}\n", .{program_header});
        print("Type: {s}\n", .{type_name});
    }

    const program_section = sections.items[program_section_index];
    const program_data = sections.items[program_data_index];

    var cpu = riscv.CPU.init(heap.allocator());
    defer cpu.deinit();

    try cpu.set_memory_size(program_section.data.items.len + program_data.data.items.len);

    // PROGRAM
    try cpu.add_memory_map(.{ //
        .start = 0,
        .end = program_section.section.sh_size - 1,
        .to_start = program_section.section.sh_addr,
        .to_end = (program_section.section.sh_addr + program_section.section.sh_size) - 1,
    });
    try cpu.vmemory_write_all(program_section.section.sh_addr, program_section.data.items);

    // DATA
    try cpu.add_memory_map(.{ //
        .start = program_section.data.items.len,
        .end = (program_section.data.items.len + program_data.data.items.len) - 1,
        .to_start = program_data.section.sh_addr,
        .to_end = (program_data.section.sh_addr + program_section.section.sh_size) - 1,
    });
    try cpu.vmemory_write_all(program_data.section.sh_addr, program_data.data.items);

    // STACK
    // try cpu.add_memory_map(.{ .start = program_section.data.items.len, .end = program_section.data.items.len + 1024, .to_start = std.math.maxInt(u64) - 1024, .to_end = std.math.maxInt(u64) });

    print("MemoryMap PROGRAM: {?}\n", .{cpu.memory_maps.items[0]});
    print("MemoryMap DATA: {?}\n", .{cpu.memory_maps.items[1]});
    {
        var memory: [4]u8 = undefined;
        var test_memory: [4]u8 = undefined;
        var p: usize = elf_header_raw.e_entry;
        while (cpu.vmemory_read_all(p, &memory)) {
            const ins = try ASM.Instr.from_memory(&memory);
            p += ins.len();
            // assambler base integrity check
            _ = try ins.to_memory(&test_memory);
            try ins.write(std.io.getStdOut().writer().any());
            if (!std.mem.eql(u8, &memory, &test_memory)) {
                dump_hex("", &memory);
                dump_hex("", &test_memory);
                return error.LossyInstruction;
            }
        } else |er| {
            print("Err: {?}\n", .{er});
        }

        std.debug.dump_hex(cpu.memory.items);
    }

    cpu.threads[0].pc = elf_header_raw.e_entry;
    // cpu.threads[0].g_regs[riscv.GeneralReg.SP.to_u5()] = program_data.section.sh_addr;

    print("Start Emulation...\n", .{});
    while (cpu.step()) {
        for (&cpu.threads, 0..) |*thread, i| {
            print("Thread: {}\n", .{i});
            const padding = "    ";
            for (&thread.g_regs, riscv.GeneralRegsNames) |*g_reg, x| {
                print("\t{s}{s} = 0x{x:0>16}\n", .{ x, padding[x.len..], g_reg.* });
            }
            print("\tPC   = {x}\n", .{thread.pc});
            print("\tCSRs:\n", .{});

            dump_hex("\t\t", std.mem.sliceAsBytes(thread.csrs[0..8]));
        }

        {
            print("MEM:\n", .{});
            std.debug.dump_hex(cpu.memory.items);
        }
    } else |e| {
        print("Finish with: {?}\n", .{e});
    }
}

pub fn dump_hex(pad: []const u8, bytes: []const u8) void {
    const stderr_mutex = std.debug.getStderrMutex();
    stderr_mutex.lock();
    defer stderr_mutex.unlock();
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
