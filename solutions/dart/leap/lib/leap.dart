class Leap {
  bool leapYear(int year) {
    divisibleBy(int n) { return year % n == 0; }
    return divisibleBy(4) && 
      !divisibleBy(100) || 
      divisibleBy(400);
  }
}
