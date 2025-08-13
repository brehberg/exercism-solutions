module MariosMarvellousLasagna exposing (remainingTimeInMinutes)

-- Work out what time the lasagna will be ready
remainingTimeInMinutes : Int -> Int -> Int
remainingTimeInMinutes numberOfLayers actualMinutesInOven =
    let
        expectedMinutesInOven = 40
        timePerLayer = 2
        preparationTimeInMinutes = timePerLayer * numberOfLayers
    in
        preparationTimeInMinutes + expectedMinutesInOven - actualMinutesInOven