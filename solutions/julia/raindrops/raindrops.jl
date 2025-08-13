function raindrops(number)
    function rain_sound(n::Int, drop::String)
        return number % n == 0 ? drop : ""
    end

    factors = [(3, "Pling"), (5, "Plang"), (7, "Plong")]
    sound = join(rain_sound(n, drop) for (n, drop) in factors)

    return sound == "" ? string(number) : sound
end
