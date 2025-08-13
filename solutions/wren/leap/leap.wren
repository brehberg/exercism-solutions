class Year {
  static isLeap(year) {
    var isDivisible = Fn.new { |n| year % n == 0 } 
    return isDivisible.call(4) &&
      !isDivisible.call(100) || 
      isDivisible.call(400)
  }
}
