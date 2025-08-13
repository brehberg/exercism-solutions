// Define the expected oven time in minutes
let expectedMinutesInOven: Int = 40

// Calculate the remaining oven time in minutes
func remainingMinutesInOven(elapsedMinutes: Int) -> Int {
  return expectedMinutesInOven - elapsedMinutes
}

// Calculate the preparation time in minutes
func preparationTimeInMinutes(layers: Int) -> Int {
  return layers * 2
}

// Calculate the total working time in minutes
func totalTimeInMinutes(layers: Int, elapsedMinutes: Int) -> Int {
  return preparationTimeInMinutes(layers: layers) + elapsedMinutes
}
