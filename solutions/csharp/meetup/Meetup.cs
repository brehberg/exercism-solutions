using System;
using System.Collections.Generic;

public enum Schedule
{
    Teenth,
    First,
    Second,
    Third,
    Fourth,
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
            ? adjustDayInWeek(new DateTime(year, month, initialDay[schedule]), dayOfWeek, 1)
            : adjustDayInWeek(new DateTime(year, month, DateTime.DaysInMonth(year, month)), dayOfWeek, -1);

    private DateTime adjustDayInWeek(DateTime guess, DayOfWeek dayOfWeek, int direction) =>
        guess.DayOfWeek == dayOfWeek ? guess
            : guess.AddDays(((int)dayOfWeek - (int)guess.DayOfWeek + 7 * direction) % 7);

    private int month { get; }
    private int year { get; }

    private static readonly Dictionary<Schedule, int> initialDay = new()
    {
        {Schedule.Teenth, 13},
        {Schedule.First, 1},
        {Schedule.Second, 8},
        {Schedule.Third, 15},
        {Schedule.Fourth, 22},
    };
}