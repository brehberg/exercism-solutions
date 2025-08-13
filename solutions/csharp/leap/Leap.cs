using System;

public static class Leap
{
    public static bool IsLeapYear(int year)
    {
        bool isDivisibleBy(int n) => year % n == 0;
        return isDivisibleBy(4) && !isDivisibleBy(100) || isDivisibleBy(400);
    }
}