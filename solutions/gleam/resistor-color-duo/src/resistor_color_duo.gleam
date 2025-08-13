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

pub fn value(colors: List(Color)) -> Result(Int, Nil) {
  case colors {
    [c1, c2, _] -> Ok(calculate(c1, c2))
    [c1, c2] -> Ok(calculate(c1, c2))
    _ -> Error(Nil)
  }
}

// Calculate a resistance value from two colors
fn calculate(color1: Color, color2: Color) -> Int {
  code(color1) * 10 + code(color2)
}

// Return the value of a color band
fn code(color: Color) -> Int {
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
