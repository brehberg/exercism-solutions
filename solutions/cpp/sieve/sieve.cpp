#include "sieve.h"

// Sieve of Eratosthenes algorithm for finding all prime numbers up to the given limit.
namespace sieve
{
    using namespace std;

    vector<int> primes(int limit)
    {
        vector<int> primes;
        vector<bool> marked(limit + 1);

        for (int i = 2; i <= limit; i++)
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
