function sieve(limit::Integer)
    primes = fill(true, limit)
    primes[1] = false
    for i = 2:limit, j = 2i:i:limit
        primes[j] = false
    end
    findall(primes)
end
