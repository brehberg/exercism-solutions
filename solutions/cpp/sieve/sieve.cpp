#include "sieve.h"

// Sieve of Eratosthenes algorithm for finding all prime numbers up to the given limit.
namespace sieve
{
    using namespace std;

    vector<int> primes(int limit)
    {
        vector<int> primes;
        vector<bool> list(limit + 1);

        for (int i = 2; i < limit; i++)
        {
            if (!list[i])
            {
                for (int j = i * 2; j <= limit; j += i)
                {
                    list[j] = true;
                }
                primes.emplace_back(i);
            }
        }
        return primes;
    }
} // namespace sieve
