using System;

static class LogLine
{
    public static string Message(string logLine)
    {
        int offset = logLine.IndexOf("]:") + "]:".Length;
        return logLine.Substring(offset).Trim();
    }

    public static string LogLevel(string logLine)
    {
        int start = logLine.IndexOf("[") + 1;
        int end = logLine.IndexOf("]") - 1;
        return logLine.Substring(start, end).ToLower();
    }

    public static string Reformat(string logLine) =>
        $"{Message(logLine)} ({LogLevel(logLine)})";
}
