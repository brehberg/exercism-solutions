public function isLeapYear(int year) returns boolean {
    var isDivisibleBy = function(int n) returns boolean {
        return year % n == 0;
    };
    return isDivisibleBy(4) && !isDivisibleBy(100) || isDivisibleBy(400);
}
