class Squares {
  private var n: Int
  init(_ maxN: Int) { n = maxN }

  var squareOfSum: Int {
    let sum = (n * (n + 1) / 2)
    return sum * sum
  }

  var sumOfSquares: Int {
    (n * (n + 1) * (2 * n + 1)) / 6
  }

  var differenceOfSquares: Int {
    squareOfSum - sumOfSquares
  }
}
