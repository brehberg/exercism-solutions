using System;
using System.Linq;

public class Anagram
{
    private string subject;
    private char[] baseLetters;

    public Anagram(string baseWord)
    {
        subject = baseWord.ToLower();
        baseLetters = sorted(subject);
    }

    public string[] FindAnagrams(string[] potentialMatches) =>
        // returns all candidates that are anagrams of, but not equal to, 'subject'
        potentialMatches.Where(word =>
        {
            if (word.Length != subject.Length) return false;

            var candidate = word.ToLower();
            if (candidate == subject) return false;

            return sorted(candidate).SequenceEqual(baseLetters);

        }).ToArray();

    private char[] sorted(string input)
    {
        var letters = input.ToCharArray();
        Array.Sort(letters);
        return letters;
    }
}