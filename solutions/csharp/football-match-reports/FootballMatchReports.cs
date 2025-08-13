using System;

public static class PlayAnalyzer
{
    public static string AnalyzeOnField(int shirtNum)
    {
        //  Output descriptions of the players based on their shirt number
        switch (shirtNum)
        {
            case 1:
                return "goalie";
            case 2:
                return "left back";
            case 3:
            case 4:
                return "center back";
            case 5:
                return "right back";
            case 6:
            case 7:
            case 8:
                return "midfielder";
            case 9:
                return "left wing";
            case 10:
                return "striker";
            case 11:
                return "right wing";
            default:
                // Raise an alert if an unknown shirt number is encountered
                throw new ArgumentOutOfRangeException($"Unknown shirt number {shirtNum}");
        }
    }

    public static string AnalyzeOffField(object report)
    {
        // Extend the coverage to include off field activity
        switch (report)
        {
            case int number:
                return $"There are {number} supporters at the match.";
            case string announcement:
                return announcement;

            // Report on incidents during the match
            case Injury injury:
                return $"Oh no! {injury.GetDescription()} Medics are on the field.";
            case Incident incident:
                return incident.GetDescription();

            // Report on club managers
            case Manager manager when manager.Club == null:
                return $"{manager.Name}";
            case Manager manager:
                return $"{manager.Name} ({manager.Club})";
            default:
                throw new ArgumentException();
        }
    }
}
