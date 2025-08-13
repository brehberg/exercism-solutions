return function(n)
    local function divisible_by(factor)
        return n % factor == 0
    end

    local sound = ""
    if divisible_by(3) then sound = sound .. "Pling" end
    if divisible_by(5) then sound = sound .. "Plang" end
    if divisible_by(7) then sound = sound .. "Plong" end
    if #sound == 0 then sound = tostring(n) end
    return sound
end
