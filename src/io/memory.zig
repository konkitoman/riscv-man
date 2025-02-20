slice: []u8,

pub fn init(slice: []u8) @This() {
    return .{ .slice = slice };
}

pub fn read(self: *@This(), index: u64, buffer: []u8) void {
    @memcpy(buffer, self.slice[index .. index + buffer.len]);
}

pub fn write(self: *@This(), index: u64, buffer: []const u8) void {
    @memcpy(self.slice[index .. index + buffer.len], buffer);
}

pub fn size(self: *@This()) u64 {
    return self.slice.len;
}
