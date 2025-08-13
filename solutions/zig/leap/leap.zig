pub fn isLeapYear(year: u32) bool {
    return isDivisibleBy(year, 4) and
        !isDivisibleBy(year, 100) or
        isDivisibleBy(year, 400);
}

fn isDivisibleBy(year: u32, n: u32) bool {
    return year % n == 0;
}
