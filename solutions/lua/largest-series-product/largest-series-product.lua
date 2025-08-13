return function(config)
    assert(config.span >= 0)
    assert(config.span <= #config.digits)
    assert(tonumber(config.digits))

    if config.span == 0 then
        return 1
    end

    local largest = -1
    for i = 0, #config.digits - config.span do
        local product = 1
        for j = i + 1, i + config.span do
            product = product * tonumber(config.digits:sub(j, j))
        end
        largest = math.max(largest, product)
    end
    return largest
end
