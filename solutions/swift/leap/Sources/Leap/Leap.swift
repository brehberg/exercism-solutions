class Year {
  private var year: Int
  init(calendarYear: Int) { year = calendarYear }
  var isLeapYear: Bool {
    year.isMultiple(of: 4) && !year.isMultiple(of: 100) || year.isMultiple(of: 400)
  }
}
