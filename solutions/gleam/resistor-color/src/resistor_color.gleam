pub type Color {
  Black
  Brown
  Red
  Orange
  Yellow
  Green
  Blue
  Violet
  Grey
  White
}

// Return the value of a color band
pub fn code(color: Color) -> Int {
  case color {
    Black -> 0
    Brown -> 1
    Red -> 2
    Orange -> 3
    Yellow -> 4
    Green -> 5
    Blue -> 6
    Violet -> 7
    Grey -> 8
    White -> 9
  }
}

// Better Be Right Or Your Great Big Values Go Wrong
pub fn colors() -> List(Color) {
  [Black, Brown, Red, Orange, Yellow, Green, Blue, Violet, Grey, White]
}
