class Collatz {
  static steps(n) {
    if (n < 1) Fiber.abort("Only positive integers are allowed")
    var steps = 0
    // Hailstone sequence.
    while (n != 1) {
      if (n % 2 == 0) {
        n = n / 2
      } else {
        n = 3 * n + 1
      }
      steps = steps + 1
    }
    return steps
  }
}
