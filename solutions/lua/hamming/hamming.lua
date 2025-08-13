local Hamming = {}

function Hamming.compute(a, b)
    if #a ~= #b then
        return -1 -- different lengths
    end

    -- Calculate the Hamming Distance between two DNA strands.
    local distance = 0
    for i = 0, #a do
        if a:sub(i, i) ~= b:sub(i, i) then
            distance = distance + 1
        end
    end
    return distance
end

return Hamming
