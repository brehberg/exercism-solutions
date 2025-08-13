#include "sieve.h"

// Sieve of Eratosthenes algorithm for finding all prime numbers up to the given limit.
namespace sieve
{
    using namespace std;

    vector<int> primes(int limit)
    {
        if (limit < 2)
        {
            return vector<int>{};
        }

        vector<int> primes{2};
        vector<bool> marked(limit + 1);

        for (int i = 3; i <= limit; i += 2)
        {
            if (marked[i])
            {
                continue;
            }
            for (int j = i * i; j <= limit; j += i)
            {
                marked[j] = true;
            }
            primes.emplace_back(i);
        }
        return primes;
    }
} // namespace sieve
