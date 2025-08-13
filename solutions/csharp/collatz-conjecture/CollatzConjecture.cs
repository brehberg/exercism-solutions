using System;

public static class CollatzConjecture
{
    public static int Steps(int number)
    {
        if (number < 1)
            throw new ArgumentOutOfRangeException(nameof(number));

        int steps = 0;
        int n = number;
        while (n > 1)
        {
            steps++;
            n = (n % 2 == 0) ? n / 2 : 3 * n + 1;
        }
        return steps;
    }
}
