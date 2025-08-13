// Define if bird gets bonus points
func bonusPoints(powerUpActive: Bool, touchingEagle: Bool) -> Bool {
  return powerUpActive && touchingEagle
}

// Define if bird scores
func score(touchingPowerUp: Bool, touchingSeed: Bool) -> Bool {
  return touchingPowerUp || touchingSeed
}

// Define if bird loses
func lose(powerUpActive: Bool, touchingEagle: Bool) -> Bool {
  !powerUpActive && touchingEagle
}

// Define if bird wins
func win(hasPickedUpAllSeeds: Bool, powerUpActive: Bool, touchingEagle: Bool) -> Bool {
  hasPickedUpAllSeeds && !lose(powerUpActive: powerUpActive, touchingEagle: touchingEagle)
}
