using System;
using System.Text.RegularExpressions;

public static class PigLatin
{
    // If a word begins with a vowel sound, add an "ay" sound to the end of the word
    private const string vowelWord = @"(?:^|\s+)(?:[aeiou]|xr|yt)(\S*)";
    private const string vowelResult = "$0ay";

    // If a word begins with a consonant sound, move it to the end of the word and then add "ay"
    private const string otherWord = @"(^|\s+)(.*qu|[^aeiou]+)([aeiouy]\S*)";
    private const string otherResult = "$1$3$2ay";

    public static string Translate(string phrase) =>
        Regex.IsMatch(phrase, vowelWord)
            ? Regex.Replace(phrase, vowelWord, vowelResult)
            : Regex.Replace(phrase, otherWord, otherResult);
}