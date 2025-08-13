using System;
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

    public static bool IsPaired(string input)
    {
        var closerNeeded = new Stack();
        foreach (var c in input)
        {
            if (matches.ContainsKey(c))
            {
                // opening bracket was found, add matching closing value to the stack
                closerNeeded.Push(matches[c]);
            }
            else if (matches.ContainsValue(c))
            {
                // closing bracket was found, is it the next expected value on stack?
                if (closerNeeded.Count == 0 || (char)closerNeeded.Pop() != c)
                    return false;
            }
        }
        return closerNeeded.Count == 0;
    }
}
