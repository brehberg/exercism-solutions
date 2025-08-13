// Protect the recovered secret plans with a password
func protectSecret(_ secret: String, withPassword password: String) -> (String) -> String {
  func passwordChecker(_ guess: String) -> String {
    return guess == password ? secret : "Sorry. No hidden secrets here."
  }
  return passwordChecker
}

// Generate a combination to open their safe
func generateCombination(forRoom room: Int, usingFunction f: (Int) -> Int) -> (Int, Int, Int) {
  let first: Int = f(room)
  let second: Int = f(first)
  let third: Int = f(second)
  return (first, second, third)
}
