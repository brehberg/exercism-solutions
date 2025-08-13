class Hamming {
  static compute(first, second) {
    if (first.count  != second.count) {
      Fiber.abort("strands must be of equal length")
    }

    var distance = 0
    var index = 0
    while (index < first.count) {
      if (first[index] != second[index]) {
        distance = distance + 1
      }
      index = index + 1
    }
    return distance
  }
}
