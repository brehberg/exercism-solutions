using System;
using System.Text;

public static class Identifier
{
    public static string Clean(string identifier)
    {
        bool isLowerCaseGreek(char c) => c >= 'α' && c <= 'ω';
        bool kebabFound = false;
        var result = new StringBuilder();
        foreach (char c in identifier)
        {
            result.Append(c switch
            {
                // Replace any spaces encountered with underscores
                _ when char.IsWhiteSpace(c) => '_',
                // Replace control characters with the upper case string "CTRL"
                _ when char.IsControl(c) => "CTRL",
                // Convert kebab-case to camelCase
                _ when kebabFound => char.ToUpper(c),
                // Omit Greek lower case letters
                _ when isLowerCaseGreek(c) => default,
                // Omit characters that are not letters
                _ when !char.IsLetter(c) => default,
                _ => c,
            });
            kebabFound = c.Equals('-');
        }
        return result.ToString();
    }
}
