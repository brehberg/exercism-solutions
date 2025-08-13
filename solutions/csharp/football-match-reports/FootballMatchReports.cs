using System;

public static class PlayAnalyzer
{
    public static string AnalyzeOnField(int shirtNum)
    {
        return shirtNum switch
        {
            //  Output descriptions of the players based on their shirt number
            1 => "goalie",
            2 => "left back",
            3 or 4 => "center back",
            5 => "right back",
            6 or 7 or 8 => "midfielder",
            9 => "left wing",
            10 => "striker",
            11 => "right wing",

            // Raise an alert if an unknown shirt number is encountered
            _ => throw new ArgumentOutOfRangeException($"Unknown shirt number {shirtNum}"),
        };
    }

    public static string AnalyzeOffField(object report)
    {
        return report switch
        {
            // Extend the coverage to include off field activity
            int => $"There are {report} supporters at the match.",
            string => $"{report}",

            // Report on incidents during the match
            Injury injury => $"Oh no! {injury.GetDescription()} Medics are on the field.",
            Incident incident => incident.GetDescription(),

            // Report on club managers
            Manager { Club: null } manager => $"{manager.Name}",
            Manager manager => $"{manager.Name} ({manager.Club})",
            _ => throw new ArgumentException(),
        };
    }
}
