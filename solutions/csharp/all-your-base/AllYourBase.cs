using System;
using System.Collections.Generic;

public static class AllYourBase
{
    public static int[] Rebase(int inputBase, int[] inputDigits, int outputBase)
    {
        if (inputBase < 2) throw new ArgumentException("invalid input base");
        if (outputBase < 2) throw new ArgumentException("invalid output base");

        // convert sequence of digits in input base to whole integer value
        var value = 0;
        foreach (var digit in inputDigits)
        {
            if (digit < 0 || digit >= inputBase) throw new ArgumentException("digit out of range");
            value *= inputBase;
            value += digit;
        }

        // convert whole integer value to sequence of digits in output base
        var outputDigits = new List<int> { };
        while (value >= outputBase)
        {
            outputDigits.Add(value % outputBase);
            value /= outputBase;
        }
        outputDigits.Add(value);
        outputDigits.Reverse();

        return outputDigits.ToArray();
    }
}