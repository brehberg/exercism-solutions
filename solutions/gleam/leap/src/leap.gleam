pub fn is_leap_year(year: Int) -> Bool {
  let is_divisible_by = fn(n: Int) -> Bool { year % n == 0 }
  is_divisible_by(4) && !is_divisible_by(100) || is_divisible_by(400)
}
