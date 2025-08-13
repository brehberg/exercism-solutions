using System;
using System.Collections.Generic;

public static class PascalsTriangle
{
    public static IEnumerable<IEnumerable<int>> Calculate(int rows)
    {
        var result = new List<int[]> { };

        while (rows > 0)
        {
            if (result.Count == 0)
            {
                result.Add(new int[] { 1 });
            }
            else
            {
                var nextRow = new List<int> { 1, 1 };
                result.Add(nextRow.ToArray());
            }
            rows -= 1;
        }

        return result.ToArray();
    }

}