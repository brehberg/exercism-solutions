"""
    is_leap_year(year)

Return `true` if `year` is a leap year in the gregorian calendar.

"""
function is_leap_year(year::Int)
    function is_divisible_by(n::Int)
        return year % n == 0
    end
    # Occurs on every year that is evenly divisible by 4
    # except every year that is evenly divisible by 100
    # unless the year is also evenly divisible by 400.
    return is_divisible_by(4) &&
           !is_divisible_by(100) ||
           is_divisible_by(400)
end

