using System;
using System.Globalization;

public static class HighSchoolSweethearts
{
    // 1. Display the couple's name separated by a heart
    public static string DisplaySingleLine(string studentA, string studentB) =>
        $"{studentA,29} ♡ {studentB,-29}";

    // 2. Display the couple's initials in an ascii art heart
    public static string DisplayBanner(string studentA, string studentB)
    {
        string bannerTemplate =
@"
     ******       ******
   **      **   **      **
 **         ** **         **
**            *            **
**                         **
**     {0}  +  {1}     **
 **                       **
   **                   **
     **               **
       **           **
         **       **
           **   **
             ***
              *
";
        return string.Format(bannerTemplate, studentA.Trim(), studentB.Trim());
    }

    // 3. German exchange students should be made to feel at home with locale-sensitive declarations.
    public static string DisplayGermanExchangeStudents(string studentA
        , string studentB, DateTime start, float hours)
    {
        string template = "{0} and {1} have been dating since {2:d} - that's {3:N2} hours";
        return string.Format(new CultureInfo("de-DE"), template, studentA, studentB, start, hours);
    }
}
