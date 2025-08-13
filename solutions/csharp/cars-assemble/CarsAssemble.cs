using System;

static class AssemblyLine
{
    private static int BaseCarsPerHour = 221;

    public static double SuccessRate(int speed)
    {
        double rate = 0.0;

        if (speed >= 1 && speed <= 4) { rate = 1.00; }
        else if (speed >= 5 && speed <= 8) { rate = 0.90; }
        else if (speed == 9) { rate = 0.80; }
        else if (speed == 10) { rate = 0.77; }

        return rate;
    }

    public static double ProductionRatePerHour(int speed) =>
        BaseCarsPerHour * speed * SuccessRate(speed);

    public static int WorkingItemsPerMinute(int speed) =>
        (int)Math.Floor(ProductionRatePerHour(speed) / 60);
}
