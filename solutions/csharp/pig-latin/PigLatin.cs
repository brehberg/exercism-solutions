using System;
using System.Text;
using System.Text.RegularExpressions;

public static class PigLatin
{
    // If a word begins with a vowel sound, add an "ay" sound to the end of the word
    private const string vowelWord = @"^(?:[aeiou]|xr|yt)(\S*)";
    private const string vowelResult = "$0ay";

    // If a word begins with a consonant sound, move it to the end of the word and then add "ay"
    private const string otherWord = @"^(.*qu|[^aeiou]+)([aeiouy]\S*)";
    private const string otherResult = "$2$1ay";

    public static string Translate(string phrase)
    {
        StringBuilder result = new StringBuilder();

        foreach (var word in Regex.Split(phrase, @"\s"))
        {
            var ordway = Regex.IsMatch(word, vowelWord)
                    ? Regex.Replace(word, vowelWord, vowelResult)
                    : Regex.Replace(word, otherWord, otherResult);

            result.Append(" " + ordway);
        }

        return result.ToString().Substring(1);
    }
}