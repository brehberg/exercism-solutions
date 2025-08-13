using System;
using System.Text.RegularExpressions;

public static class PhoneNumber
{
    public static (bool IsNewYork, bool IsFake, string LocalNumber) Analyze(string phoneNumber) =>
        (IsNewYork: Regex.IsMatch(phoneNumber, @"^212-")
        , IsFake: Regex.IsMatch(phoneNumber, @"-555-")
        , LocalNumber: Regex.Match(phoneNumber, @"\d{4}$").Value
        );

    public static bool IsFake((bool IsNewYork, bool IsFake, string LocalNumber) phoneNumberInfo) =>
        phoneNumberInfo.IsFake;
}
