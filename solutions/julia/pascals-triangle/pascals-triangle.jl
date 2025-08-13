function triangle(n:Int)
    n < 0 && throw(DomainError(n, "n cannot be negative"))
    n == 0 && return []

    result = []
    i = 0
    while i <= (n - 1)
        row = Int[]
        for j = 0:i
            push!(row, binomial(i, j))
        end
        push!(result, row)
        i += 1
    end
    result
end
