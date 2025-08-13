using System;
using System.Linq;

public static class RomanNumeralExtension
{
    public static string ToRoman(this int value) =>
        string.Join("", Enumerable.Repeat("I", value))
        // replace I with V, X patterns
        .Replace("IIIII", "V")
        .Replace("IIII", "IV")
        .Replace("VV", "X")
        .Replace("VIV", "IX")
        // replace X with L, C patterns
        .Replace("XXXXX", "L")
        .Replace("XXXX", "XL")
        .Replace("LL", "C")
        .Replace("LXL", "XC")
        // replace C with D, M patterns
        .Replace("CCCCC", "D")
        .Replace("CCCC", "CD")
        .Replace("DD", "M")
        .Replace("DCD", "CM");
}