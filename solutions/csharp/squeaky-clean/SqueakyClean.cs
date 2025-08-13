using System;
using System.Text;

public static class Identifier
{
    public static string Clean(string identifier)
    {
        StringBuilder result = new StringBuilder();
        bool kebabFound = false;
        foreach (char c in identifier)
        {
            // Replace any spaces encountered with underscores
            if (char.IsWhiteSpace(c)) { result.Append('_'); }
            // Replace control characters with the upper case string "CTRL"
            else if (char.IsControl(c)) { result.Append("CTRL"); }
            // Convert kebab-case to camelCase            
            else if (c == '-') { kebabFound = true; }
            else if (kebabFound)
            {
                kebabFound = false;
                result.Append(char.ToUpper(c));
            }
            // Omit Greek lower case letters
            else if (c >= 'α' && c <= 'ω') { continue; }
            // Omit characters that are not letters
            else if (char.IsLetter(c)) { result.Append(c); }

        }
        return result.ToString();
    }
}
