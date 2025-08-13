using System;

public static class ResistorColorDuo
{
    public static int Value(string[] colors) =>
        Array.IndexOf(ResistorColorDuo.colors, colors[0]) * 10
        + Array.IndexOf(ResistorColorDuo.colors, colors[1]);

    // Better Be Right Or Your Great Big Values Go Wrong
    private static string[] colors =
        { "black", "brown", "red", "orange", "yellow",
          "green", "blue", "violet", "grey", "white" };
}
