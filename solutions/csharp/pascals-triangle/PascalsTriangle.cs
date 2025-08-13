using System;
using System.Collections.Generic;

public static class PascalsTriangle
{
    public static IEnumerable<IEnumerable<int>> Calculate(int rows)
    {
        var row = new List<int> { 1 };

        while (rows > 0)
        {
            yield return row.ToArray();
            row = generateNextRow(row);
            rows--;
        }
    }

    private static List<int> generateNextRow(List<int> previousRow)
    {
        var nextRow = new List<int> { 1 };
        for (int i = 1; i < previousRow.Count; i++)
        {
            nextRow.Add(previousRow[i - 1] + previousRow[i]);
        }
        nextRow.Add(1);
        return nextRow;
    }
}