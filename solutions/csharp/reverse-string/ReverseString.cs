using System;
using System.Text;

public static class ReverseString
{
    public static string Reverse(string input)
    {
        StringBuilder sb = new StringBuilder();
        foreach (var c in input)
        {
            sb.Insert(0, c);
        }
        return sb.ToString();
    }
}
