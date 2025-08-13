local leap_year = function(year)
    local function divisible_by(n)
        return year % n == 0
    end
    return divisible_by(4) 
        and not divisible_by(100) 
        or divisible_by(400)
end

return leap_year
