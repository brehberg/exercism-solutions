using System;
using System.Linq;

public static class LogAnalysis
{
    // Allow retrieving the string after a specific substring
    public static string SubstringAfter(this string line, string sub) =>
        line.Substring(line.IndexOf(sub) + sub.Length);

    // Allow retrieving the string in between two substrings
    public static string SubstringBetween(this string line, string start, string end) =>
        line.Split(end).First().Split(start).Last();

    // Parse message in a log
    public static string Message(this string logLine) =>
        logLine.SubstringAfter(": ").Trim();

    // Parse log level in a log
    public static string LogLevel(this string logLine) =>
        logLine.SubstringBetween("[", "]").ToUpper();
}