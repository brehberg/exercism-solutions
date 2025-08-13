using System;

public static class BinarySearch
{
    public static int Find(int[] input, int value)
    {
        var low = 0;
        var high = input.Length - 1;

        while (low <= high)
        {
            var mid = low + (high - low) / 2;

            if (input[mid] < value)
            {
                low = mid + 1;
            }
            else if (input[mid] > value)
            {
                high = mid - 1;
            }
            else
            {
                return mid;
            }
        }
        return -1;
    }
}