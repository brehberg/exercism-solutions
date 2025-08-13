using System;
using System.Collections.Generic;

public static class ListOps
{
    public static int Length<T>(List<T> input)
    {
        int count = 0;
        foreach (var _ in input)
        {
            count += 1;
        }
        return count;
    }

    public static List<T> Reverse<T>(List<T> input)
    {
        var final = new List<T>();
        foreach (var item in input)
        {
            final.Insert(0, item);
        }
        return final;
    }

    public static List<TOut> Map<TIn, TOut>(List<TIn> input, Func<TIn, TOut> map)
    {
        var final = new List<TOut>();
        foreach (var item in input)
        {
            final.Add(map(item));
        }
        return final;
    }

    public static List<T> Filter<T>(List<T> input, Func<T, bool> predicate)
    {
        var final = new List<T>();
        foreach (var item in input)
        {
            if (predicate(item)) { final.Add(item); }
        }
        return final;
    }

    public static TOut Foldl<TIn, TOut>(List<TIn> input, TOut start, Func<TOut, TIn, TOut> func)
    {
        var result = start;
        foreach (var item in input)
        {
            result = func(result, item);
        }
        return result;
    }

    public static TOut Foldr<TIn, TOut>(List<TIn> input, TOut start, Func<TIn, TOut, TOut> func)
    {
        var result = start;
        foreach (var item in Reverse(input))
        {
            result = func(item, result);
        }
        return result;
    }

    public static List<T> Concat<T>(List<List<T>> input)
    {
        var final = new List<T>();
        foreach (var list in input)
        {
            Append(final, list);
        }
        return final;
    }

    public static List<T> Append<T>(List<T> left, List<T> right)
    {
        var final = left;
        foreach (var item in right)
        {
            final.Add(item);
        }
        return final;
    }
}