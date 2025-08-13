using System;

static class SavingsAccount
{
    public static float InterestRate(decimal balance)
    {
        // Calculate the interest rate
        switch (balance)
        {
            case < 0.00m:
                return 3.213f;
            case < 1000.00m:
                return 0.5f;
            case < 5000.00m:
                return 1.621f;
            default:
                return 2.475f;
        }
    }

    public static decimal Interest(decimal balance)
    {
        // Calculate the interest
        return balance * (decimal)(InterestRate(balance) / 100);
    }

    public static decimal AnnualBalanceUpdate(decimal balance)
    {
        // Calculate the annual balance update
        return balance + Interest(balance);
    }

    public static int YearsBeforeDesiredBalance(decimal balance, decimal targetBalance)
    {
        // Calculate the years before reaching the desired balance
        var years = 0;
        while (balance < targetBalance)
        {
            years += 1;
            balance = AnnualBalanceUpdate(balance);
        }
        return years;
    }
}
