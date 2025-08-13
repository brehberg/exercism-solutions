using System;
using System.Collections.Generic;

public enum Schedule
{
    Teenth = 13,
    First = 1,
    Second = 8,
    Third = 15,
    Fourth = 22,
    Last
}

public class Meetup
{
    public Meetup(int month, int year)
    {
        this.month = month;
        this.year = year;
    }

    public DateTime Day(DayOfWeek dayOfWeek, Schedule schedule) =>
        schedule != Schedule.Last
            ? adjustDayInWeek(dayOfWeek, new DateTime(year, month, (int)schedule), 1)
            : adjustDayInWeek(dayOfWeek, new DateTime(year, month, DateTime.DaysInMonth(year, month)), -1);

    private DateTime adjustDayInWeek(DayOfWeek dayOfWeek, DateTime guess, int direction) =>
        dayOfWeek == guess.DayOfWeek ? guess
            : guess.AddDays(((int)dayOfWeek - (int)guess.DayOfWeek + 7 * direction) % 7);

    private readonly int month;
    private readonly int year;
}