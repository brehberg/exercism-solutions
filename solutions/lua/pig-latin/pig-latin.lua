return function(phrase)
    local result = ""
    for word in phrase:gmatch("%S+") do
        if word:find("^[aeiou]") or word:find("^xr") or word:find("^yt") then
            result = result .. " " .. word .. "ay" -- Rule 1
        elseif word:find("^(.?qu)(.*)") then
            result = result .. " " .. word:gsub("^(.?qu)(.*)", "%2%1ay") -- Rule 3
        elseif word:find("^([^aeiou]+)(y.*)") then
            result = result .. " " .. word:gsub("^([^aeiou]+)(y.*)", "%2%1ay") -- Rule 4
        else
            result = result .. " " .. word:gsub("^([^aeiou]+)(.*)", "%2%1ay") -- Rule 2
        end
    end
    return result:sub(2)
end
