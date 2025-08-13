function valid_range(n)
    return n >= 0 and n <= 7
end

function valid_position(pos)
    return valid_range(pos.row) and valid_range(pos.column)
end

function diagonal(pos, oth)
    return math.abs(pos.row - oth.row) == math.abs(pos.column - oth.column)
end

return function(pos)
    if not valid_position(pos) then error("invalid position") end

    pos.can_attack = function(oth)
        return pos.row == oth.row 
            or pos.column == oth.column
            or diagonal(pos, oth)
    end

    return pos
end

