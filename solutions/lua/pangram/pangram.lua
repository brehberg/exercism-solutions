return function(sentence)
    local letters = {}
    local found = 0

    for char in sentence:lower():gmatch("%l") do
        if not letters[char] then
            letters[char] = true
            found = found + 1
        end
        if found == 26 then
            return true
        end
    end

    return false
end
