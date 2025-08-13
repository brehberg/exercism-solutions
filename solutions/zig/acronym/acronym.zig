const std = @import("std");
const mem = std.mem;
const ascii = std.ascii;

pub fn abbreviate(allocator: mem.Allocator, words: []const u8) mem.Allocator.Error![]u8 {
    var result: std.ArrayList(u8) = std.ArrayList(u8).init(allocator);
    var prev: u8 = ' ';

    for (words) |char| {
        if (!isSeparator(char) and !ascii.isAlphabetic(char)) {
            continue;
        }

        if (isSeparator(prev) and !isSeparator(char)) {
            try result.append(ascii.toUpper(char));
        }

        prev = char;
    }

    return result.toOwnedSlice();
}

fn isSeparator(char: u8) bool {
    return char == '-' or ascii.isWhitespace(char);
}
