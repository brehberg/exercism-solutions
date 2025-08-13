using System;
using System.Collections.Generic;

public static class SecretHandshake
{
    private static readonly SortedList<byte, string> actions = new SortedList<byte, string>
    {
        {0b00001, "wink"},
        {0b00010, "double blink"},
        {0b00100, "close your eyes"},
        {0b01000, "jump"},
        {0b10000, "reverse"},
    };

    public static string[] Commands(int commandValue)
    {
        var handshake = new List<string>();
        foreach (var action in actions)
        {
            if ((action.Key & commandValue) == 0)
                continue;
            if (action.Value == "reverse")
                handshake.Reverse();
            else
                handshake.Add(action.Value);
        }
        return handshake.ToArray();
    }
}
