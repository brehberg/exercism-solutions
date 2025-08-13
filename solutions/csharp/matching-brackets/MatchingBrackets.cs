using System;
using System.Linq;
using System.Collections;
using System.Collections.Generic;

public static class MatchingBrackets
{
    private static readonly Dictionary<char, char> matches = new Dictionary<char, char>
    {
        {'[', ']'},
        {'{', '}'},
        {'(', ')'},
    };
    private static readonly List<char> openers = matches.Keys.ToList();
    private static readonly List<char> closers = matches.Values.ToList();

    public static bool IsPaired(string input)
    {
        var closerNeeded = new Stack();
        foreach (var c in input)
        {
            if (closers.Contains(c))
            {
                if (closerNeeded.Count == 0 || (char)closerNeeded.Pop() != c)
                    return false;
            }
            else if (openers.Contains(c))
            {
                closerNeeded.Push(matches[c]);
            }
        }
        return closerNeeded.Count == 0;
    }
}
