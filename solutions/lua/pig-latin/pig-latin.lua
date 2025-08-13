return function(phrase)
    local function translate_word(word)
        local part1, part2 = nil, nil

        -- Rule 1: If a word begins with a vowel, or starts with "xr" or "yt"
        if word:find("^[aeiou]") or word:find("^xr") or word:find("^yt") then
            part1, part2 = word, ""
        end
        -- Rule3: If a word starts with zero or more consonants followed by "qu",
        -- move those consonants (if any) and the "qu" to the end of the word
        if not part1 then
            part1, part2 = word:match("^([^aeiou]*qu)(.*)") -- Rule 3
        end
        -- Rule 4: If a word starts with one or more consonants followed by "y",
        -- move the consonants preceding the "y"to the end of the word
        if not part1 then
            part1, part2 = word:match("^([^aeiou]+)(y.*)") -- Rule 4
        end
        -- Rule 2: If a word begins with one or more consonants,
        -- move those consonants to the end of the word
        if not part1 then
            part1, part2 = word:match("^([^aeiou]+)(.*)") -- Rule 2
        end
        if part1 and part2 then
            return string.format("%s%say", part2, part1)
        end
    end

    return phrase:gsub("%S+", translate_word)
end
