using System;

public static class LogAnalysis
{
    // Allow retrieving the string after a specific substring
    public static string SubstringAfter(this string logLine, string sub) =>
        logLine.Substring(logLine.IndexOf(sub) + sub.Length);

    // Allow retrieving the string in between two substrings
    public static string SubstringBetween(this string logLine, string start, string end)
    {
        int startPos = logLine.IndexOf(start) + start.Length;
        int endPos = logLine.IndexOf(end) - end.Length - start.Length + 1;
        return logLine.Substring(startPos, endPos);
    }

    // Parse message in a log
    public static string Message(this string logLine) =>
        logLine.SubstringAfter(": ").Trim();

    // Parse log level in a log
    public static string LogLevel(this string logLine) =>
        logLine.SubstringBetween("[", "]").ToUpper();
}