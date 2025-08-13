local leap_year = function(number)
    local function divisible_by(n)
        return number % n == 0
    end
    return divisible_by(400) 
        or not divisible_by(100) 
        and divisible_by(4)
end

return leap_year
