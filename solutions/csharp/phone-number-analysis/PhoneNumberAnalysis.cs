using System;
using System.Text.RegularExpressions;

public static class PhoneNumber
{
    public static (bool IsNewYork, bool IsFake, string LocalNumber) Analyze(string phoneNumber)
    {
        var match = Regex.Match(phoneNumber, @"^(\d{3})-(\d{3})-(\d{4})$");
        if (!match.Success)
            throw new ArgumentException($"invalid phone number: {phoneNumber}");

        return (
            match.Groups[1].Value == "212", // has a New York dialing code
            match.Groups[2].Value == "555", // has fake 555 as a prefix code
            match.Groups[3].Value           // the last 4 digits of the number
        );
    }

    public static bool IsFake((bool IsNewYork, bool IsFake, string LocalNumber) phoneNumberInfo) =>
        phoneNumberInfo.IsFake;
}
