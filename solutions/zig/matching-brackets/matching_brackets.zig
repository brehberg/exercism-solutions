const std = @import("std");
const mem = std.mem;

// Checks that all the brackets and braces in the string
// are matched correctly, and nested correctly
pub fn isBalanced(allocator: mem.Allocator, input: []const u8) !bool {
    var matches = std.AutoHashMap(u8, u8).init(allocator);
    defer matches.deinit();
    try matches.put('[', ']');
    try matches.put('{', '}');
    try matches.put('(', ')');

    var closerNeeded = std.ArrayList(u8).init(allocator);
    defer closerNeeded.deinit();

    for (input) |char| {
        var closer = matches.get(char);
        if (closer) |c| {
            // opening bracket was found, add matching closing value to the stack
            try closerNeeded.append(c);
        } else if (mapHasValue(matches, char)) {
            // closing bracket was found, is it the next expected value on stack?
            if (closerNeeded.popOrNull() != char) {
                return false;
            }
        }
    }

    return closerNeeded.items.len == 0;
}

// determine if matching bracket map contains the specified char value
inline fn mapHasValue(map: anytype, value: u8) bool {
    var values = map.valueIterator();
    while (values.next()) |value_ptr| {
        if (value_ptr.* == value) {
            return true;
        }
    }
    return false;
}
