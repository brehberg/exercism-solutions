function triangle(n::Int)
    n < 0 && throw(DomainError(n, "n cannot be negative"))

    function generate_row(i::Int)
        return [binomial(i, j) for j in 0:i]
    end

    return [generate_row(i) for i in 0:n-1]
end
