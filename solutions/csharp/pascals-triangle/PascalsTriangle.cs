using System;
using System.Collections.Generic;

public static class PascalsTriangle
{
    public static IEnumerable<IEnumerable<int>> Calculate(int rows)
    {
        var result = new List<int[]>();
        var row = new List<int> { 1 };

        while (rows > 0)
        {
            result.Add(row.ToArray());
            row = generateNextRow(row);
            rows--;
        }
        return result.ToArray();
    }

    private static List<int> generateNextRow(List<int> row)
    {
        var nextRow = new List<int> { 1 };
        for (int i = 1; i < row.Count; i++)
        {
            nextRow.Add(row[i - 1] + row[i]);
        }
        nextRow.Add(1);
        return nextRow;
    }
}