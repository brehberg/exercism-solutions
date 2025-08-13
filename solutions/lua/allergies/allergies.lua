local c_allergens = {
    eggs = 1,
    peanuts = 2,
    shellfish = 4,
    strawberries = 8,
    tomatoes = 16,
    chocolate = 32,
    pollen = 64,
    cats = 128
}

local function allergic_to(score, which)
    return score & c_allergens[which] > 0
end

local function ordered(allergens)
    table.sort(
        allergens,
        function(a, b)
            return c_allergens[a] < c_allergens[b]
        end
    )
    return allergens
end

local function list(score)
    local allergies = {}
    for allergen in pairs(c_allergens) do
        if allergic_to(score, allergen) then
            table.insert(allergies, allergen)
        end
    end
    return ordered(allergies)
end

return {list = list, allergic_to = allergic_to}
