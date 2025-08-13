using System;
using System.Runtime.InteropServices;
using System.Globalization;

public enum Location
{
    NewYork,
    London,
    Paris
}

public enum AlertLevel
{
    Early,
    Standard,
    Late
}

public static class Appointment
{
    // 1. Provide local time equivalents of UTC (Universal Coordinated Time) appointments for the administrators
    public static DateTime ShowLocalTime(DateTime dtUtc) => dtUtc.ToLocalTime();

    // 2. Schedule appointments in New York, London and Paris
    public static DateTime Schedule(string appointmentDateDescription, Location location) =>
        TimeZoneInfo.ConvertTimeToUtc(DateTime.Parse(appointmentDateDescription), determineTimeZone(location));

    // 3. Provide alerts to clients at intervals before the appointment
    public static DateTime GetAlertTime(DateTime appointment, AlertLevel alertLevel)
    {
        var alertTimeSpan = alertLevel switch
        {
            AlertLevel.Early => new TimeSpan(1, 0, 0, 0),   // 1 day
            AlertLevel.Standard => new TimeSpan(1, 45, 0),  // 1 hour 45 minutes 
            AlertLevel.Late => new TimeSpan(0, 30, 0),      // 30 minutes
            _ => throw new ArgumentOutOfRangeException(nameof(alertLevel)),
        };
        return appointment - alertTimeSpan;
    }

    // 4. If daylight savings has recently changed we send a message to clients reminding them.
    public static bool HasDaylightSavingChanged(DateTime dt, Location location)
    {
        var tz = determineTimeZone(location);
        var ts = new TimeSpan(7, 0, 0, 0);
        return tz.IsDaylightSavingTime(dt) != tz.IsDaylightSavingTime(dt - ts);
    }

    // 5. Use the local date time format to enter appointments
    public static DateTime NormalizeDateTime(string dtStr, Location location) =>
        DateTime.TryParse(dtStr, determineCulture(location), out DateTime dt) ? dt : DateTime.MinValue;

    private static CultureInfo determineCulture(Location location)
    {
        var cultureId = location switch
        {
            Location.NewYork => "en-US",
            Location.London => "en-GB",
            Location.Paris => "fr-FR",
            _ => throw new ArgumentOutOfRangeException(nameof(location)),
        };
        return CultureInfo.GetCultureInfo(cultureId);
    }
    private static TimeZoneInfo determineTimeZone(Location location)
    {
        var timeZoneId = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
            ? windowsTimeZoneId(location) : nonWindowsTimeZoneId(location);
        return TimeZoneInfo.FindSystemTimeZoneById(timeZoneId);
    }
    private static string windowsTimeZoneId(Location location) => location switch
    {
        Location.NewYork => "Eastern Standard Time",
        Location.London => "GMT Standard Time",
        Location.Paris => "W. Europe Standard Time",
        _ => throw new ArgumentOutOfRangeException(nameof(location)),
    };
    private static string nonWindowsTimeZoneId(Location location) => location switch
    {
        Location.NewYork => "America/New_York",
        Location.London => "Europe/London",
        Location.Paris => "Europe/Paris",
        _ => throw new ArgumentOutOfRangeException(nameof(location)),
    };
}
