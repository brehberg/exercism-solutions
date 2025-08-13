interface LambdaLinterface {
    public boolean isDivisibleBy(int n);
}

class Leap {
    public boolean isLeapYear(int year) {
        LambdaLinterface check = (n) -> year % n == 0;
        return check.isDivisibleBy(4) &&
                !check.isDivisibleBy(100) ||
                check.isDivisibleBy(400);
    }
}
