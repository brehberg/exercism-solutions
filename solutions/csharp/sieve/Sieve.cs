using System;
using System.Collections;
using System.Collections.Generic;

public static class Sieve
{
    // Sieve of Eratosthenes algorithm for finding all prime numbers up to the given limit.
    public static IEnumerable<int> Primes(int limit)
    {
        if (limit < 0) throw new ArgumentOutOfRangeException(nameof(limit));

        var marked = new BitArray(limit + 1);
        for (var i = 2; i <= limit; i++)
        {
            if (marked[i]) continue;
            for (var j = i * i; j <= limit; j += i)
            {
                marked[j] = true;
            }
            yield return i;
        }
    }
}