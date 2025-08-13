local Hamming = {}

function Hamming.compute(a, b)
    if not (#a == #b) then
        return -1 -- different lengths
    end

    local diff_count = 0
    for i = 0, #a do
        if not (a:sub(i, i) == b:sub(i, i)) then
            diff_count = diff_count + 1
        end
    end
    return diff_count
end

return Hamming
