return function(sentence)
    local letters = {}
    for char in sentence:lower():gmatch("%l") do
        if letters[char] then
            return false
        end
        letters[char] = true
    end
    return true
end
