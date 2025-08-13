module LuciansLusciousLasagna exposing (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes)

-- Define the expected oven time in minutes
expectedMinutesInOven = 40

-- Calculate the preparation time in minutes
timePerLayer = 2
preparationTimeInMinutes numberOfLayers =
    timePerLayer * numberOfLayers

-- Calculate the elapsed time in minutes
elapsedTimeInMinutes numberOfLayers actualMinutesInOven =
    preparationTimeInMinutes numberOfLayers + actualMinutesInOven
