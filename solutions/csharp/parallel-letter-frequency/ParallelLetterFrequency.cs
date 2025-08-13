using System;
using System.Collections.Generic;
using System.Linq;

public static class ParallelLetterFrequency
{
    public static Dictionary<char, int> Calculate(IEnumerable<string> texts) =>
        texts.AsParallel().Aggregate(new Dictionary<char, int>(), letterFrequency);

    private static Dictionary<char, int> letterFrequency(Dictionary<char, int> frequencies, string text) =>
        text.ToLower().Where(char.IsLetter).Aggregate(frequencies, addLetterFrequency);

    private static Dictionary<char, int> addLetterFrequency(Dictionary<char, int> frequency, char letter)
    {
        frequency.TryAdd(letter, 0);
        frequency[letter] += 1;
        return frequency;
    }
}