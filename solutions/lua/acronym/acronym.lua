return function(s)
    local acronym = ""
    -- insert a space between a lower and an upper
    local phrase = s:gsub("([%l])([%u])", "%1 %2")

    -- split into words after each space or hyphen
    for word in phrase:gmatch("[^%s|-]+") do
        acronym = acronym .. word:sub(1, 1)
    end
    return acronym:upper()
end
