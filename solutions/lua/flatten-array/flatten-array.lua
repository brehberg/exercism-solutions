local function flatten(input)
    local res = {}

    local function flatten(val)
        if type(val) == "table" then
            for _, val in ipairs(val) do
                flatten(val)
            end
        else
            table.insert(res, val)
        end
    end
    
    flatten(input)
    return res
end

return flatten
