local function reduce(xs, value, func)
    local acc = value
    for _, x in ipairs(xs) do
        acc = func(x, acc)
    end
    return acc
end

local function map(xs, func)
    local res = {}
    for _, x in ipairs(xs) do
        table.insert(res, func(x))
    end
    return res
end

local function filter(xs, pred)
    local res = {}
    for _, x in ipairs(xs) do
        if pred(x) then
            table.insert(res, x)
        end
    end
    return res
end

return {map = map, reduce = reduce, filter = filter}
