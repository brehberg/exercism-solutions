pub fn convert(number: Int) -> String {
  case number {
    n if n >= 1000 -> "M" <> convert(n - 1000)
    n if n >= 900 -> "C" <> convert(n + 100)
    n if n >= 500 -> "D" <> convert(n - 500)
    n if n >= 400 -> "C" <> convert(n + 100)
    n if n >= 100 -> "C" <> convert(n - 100)
    n if n >= 90 -> "X" <> convert(n + 10)
    n if n >= 50 -> "L" <> convert(n - 50)
    n if n >= 40 -> "X" <> convert(n + 10)
    n if n >= 10 -> "X" <> convert(n - 10)
    n if n >= 9 -> "I" <> convert(n + 1)
    n if n >= 5 -> "V" <> convert(n - 5)
    n if n >= 4 -> "I" <> convert(n + 1)
    n if n >= 1 -> "I" <> convert(n - 1)
    _ -> ""
  }
}
