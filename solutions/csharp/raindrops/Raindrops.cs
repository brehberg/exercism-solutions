using System;
using System.Text;
using System.Collections.Generic;

public static class Raindrops
{
    private static readonly Dictionary<int, string> _drops = new Dictionary<int, string> {
        { 3, "Pling" }, { 5, "Plang" }, { 7, "Plong" } };

    public static string Convert(int number)
    {
        // determine the maximum length of all possible raindrop sounds
        int maxLength = 0;
        foreach (string sound in _drops.Values)
        {
            maxLength += sound.Length;
        }

        // build string with raindrop sounds corresponding to factors
        StringBuilder sb = new StringBuilder(maxLength);
        foreach (KeyValuePair<int, string> factor in _drops)
        {
            if (number % factor.Key == 0) { sb.Append(factor.Value); }
        }

        return (sb.Length != 0) ? sb.ToString() : number.ToString();
    }
}