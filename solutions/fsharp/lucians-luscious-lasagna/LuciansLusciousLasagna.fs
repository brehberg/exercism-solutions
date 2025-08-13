module LuciansLusciousLasagna

// Define the expected oven time in minutes
let expectedMinutesInOven: int = 40

// Calculate the remaining oven time in minutes.
let remainingMinutesInOven (elapsedBakeTime: int) : int = expectedMinutesInOven - elapsedBakeTime

// Calculate the preparation time in minutes.
let preparationTimeInMinutes (numberOfLayers: int) : int =
    let preperationTimePerLayer: int = 2
    preperationTimePerLayer * numberOfLayers

// Calculate the elapsed time in minutes.
let elapsedTimeInMinutes (numberOfLayers: int) (elapsedBakeTime: int) : int =
    let prepTimeOfLayers: int = preparationTimeInMinutes numberOfLayers
    prepTimeOfLayers + elapsedBakeTime
