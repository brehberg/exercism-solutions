using System;

static class Badge
{
    // Print a badge for an employee
    public static string Print(int? id, string name, string? department) =>
        $"{Prefix(id)}{name}{Postfix(department).ToUpper()}";

    // Print a badge for a new employee
    private static string Prefix(int? id) =>
        (id == null) ? "" : $"[{id}] - ";

    // Print a badge for the owner
    private static string Postfix(string? department) =>
        $" - {department ?? "owner"}";
}
