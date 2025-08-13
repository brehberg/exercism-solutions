module LuciansLusciousLasagna exposing (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes)

-- Define the expected oven time in minutes
expectedMinutesInOven : Int
expectedMinutesInOven = 40

-- Calculate the preparation time in minutes
preparationTimeInMinutes : Int -> Int
preparationTimeInMinutes numberOfLayers =
    let timePerLayer = 2
    in timePerLayer * numberOfLayers

-- Calculate the elapsed time in minutes
elapsedTimeInMinutes : Int -> Int -> Int
elapsedTimeInMinutes numberOfLayers actualMinutesInOven =
    preparationTimeInMinutes numberOfLayers + actualMinutesInOven
