pub fn primes(buffer: []u32, limit: u32) []u32 {
    if (limit < 2) {
        return buffer[0..0];
    }

    var marked: [1001]bool = undefined;
    var i: u32 = 0;

    for (2..limit + 1) |n| {
        if (marked[n]) {
            continue;
        }
        for (n..limit + 1) |m| {
            if (m % n == 0) {
                marked[m] = true;
            }
        }
        buffer[i] = @intCast(n);
        i += 1;
    }
    return buffer[0..i];
}
