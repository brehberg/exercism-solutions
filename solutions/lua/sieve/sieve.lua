-- Sieve of Eratosthenes algorithm for lazily finding
-- all prime numbers up to the given limit.
return function(limit)
    return coroutine.create(
        function()
            local marked = {}
            for i = 2, limit do
                if marked[i] then
                    goto continue
                end
                coroutine.yield(i)
                for j = i * i, limit, i do
                    marked[j] = true
                end
                ::continue::
            end
        end
    )
end
