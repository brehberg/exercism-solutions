object Leap {
  def leapYear(year: Int): Boolean = {
    val divisibleBy = (n: Int) => { year  %  n == 0 }
    divisibleBy(4) && !divisibleBy(100) || divisibleBy(400)
  }
}
