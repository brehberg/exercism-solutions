using System;

public static class CentralBank
{
    public static string DisplayDenomination(long @base, long multiplier)
    {
        try
        {
            return checked($"{@base * multiplier}");
        }
        catch (OverflowException)
        {
            return "*** Too Big ***";
        }
    }

    public static string DisplayGDP(float @base, float multiplier)
    {
        float GDP = checked(@base * multiplier);
        return Single.IsInfinity(GDP) ? "*** Too Big ***" : $"{GDP}";
    }

    public static string DisplayChiefEconomistSalary(decimal salaryBase, decimal multiplier)
    {
        try
        {
            return checked($"{salaryBase * multiplier}");
        }
        catch (OverflowException)
        {
            return "*** Much Too Big ***";
        }
    }
}
