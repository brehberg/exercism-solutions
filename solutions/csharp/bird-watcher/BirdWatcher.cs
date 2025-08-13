using System;

class BirdCount
{
    private int[] birdsPerDay = new int[7];
    private const int _today = 6;

    public BirdCount(int[] birdsPerDay) => this.birdsPerDay = birdsPerDay;

    public static int[] LastWeek() => new[] { 0, 2, 5, 3, 7, 8, 4 };

    public int Today() => birdsPerDay[_today];

    public void IncrementTodaysCount() => birdsPerDay[_today] += 1;

    public bool HasDayWithoutBirds() => Array.IndexOf(birdsPerDay, 0) != -1;

    public int CountForFirstDays(int numberOfDays)
    {
        int sum = 0;
        for (int i = 0; i < numberOfDays; i++)
        {
            sum += birdsPerDay[i];
        }
        return sum;
    }

    public int BusyDays()
    {
        const int busyDayBirds = 5;
        int days = 0;
        foreach (var birds in birdsPerDay)
        {
            days += birds < busyDayBirds ? 0 : 1;
        }
        return days;
    }
}
