function to_roman(number)
    if number ∉ 1:3999
        throw(ErrorException("The number must be between 1 and 3999."))
    end

    unit = ("I", "X", "C", "M")
    half = ("V", "L", "D")

    function to_roman_digit(index, digit)
        if digit < 4
            unit[index]^digit
        elseif digit == 4
            unit[index] * half[index]
        elseif digit == 9
            unit[index] * unit[index+1]
        else
            half[index] * unit[index]^(digit - 5)
        end
    end

    return join(to_roman_digit(index, digit)
                for (index, digit) in Iterators.reverse(enumerate(digits(number))))
end
