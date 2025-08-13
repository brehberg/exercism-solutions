# Sieve of Eratosthenes algorithm for finding 
# all prime numbers up to the given limit.
function sieve(limit::Int)
    primes = fill(true, limit)
    primes[1] = false
    for i::Int = 2:sqrt(limit), j = i^2:i:limit
        primes[j] = false
    end
    findall(primes)
end
