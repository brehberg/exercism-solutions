using System;
using System.Collections.Generic;

using Item = (int weight, int value);

public static class Knapsack
{
    public static int MaximumValue(int maximumWeight, Item[] items) =>
        max(items.Length - 1, maximumWeight, items).value;

    private static Item max(int i, int maxWeight, Item[] items)
    {
        if (i < 0 || maxWeight == 0) return (0, 0);

        if (items[i].weight > maxWeight)    // item cannot fit in the bag
            return max(i - 1, maxWeight, items);

        var prev = max(i - 1, maxWeight, items);
        var next = max(i - 1, maxWeight - items[i].weight, items);

        next.value += items[i].value;
        if (next.value > prev.value)
        {
            next.weight += items[i].weight;
            return next;
        }
        return prev;
    }
}
