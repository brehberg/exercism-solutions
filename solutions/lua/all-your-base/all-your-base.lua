local all_your_base = {}

all_your_base.convert = function(from_digits, from_base)
    assert(from_base >= 2, 'invalid input base')
    local int = { value = 0 }

    -- convert sequence of digits in input base to whole integer value
    local sum = 0
    for i, d in ipairs(from_digits) do
        assert(d >= 0, 'negative digits are not allowed')
        assert(d < from_base, 'digit out of range')
        sum = sum + d * from_base ^ (#from_digits - i)
    end
    int.value = math.floor(sum)
    
    int.to = function(to_base)
        assert(to_base >= 2, 'invalid output base')
        local digits = { }
    
        -- convert whole integer value to sequence of digits in output base
        local rem = int.value
        while rem >= to_base do
            table.insert(digits, 1, rem % to_base)
            rem = rem // to_base
        end
        table.insert(digits, 1, rem)
        
        return digits
    end

    return int
end

return all_your_base
