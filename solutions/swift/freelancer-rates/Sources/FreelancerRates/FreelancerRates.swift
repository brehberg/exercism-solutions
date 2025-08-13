let workHoursPerDay: Double = 8
let billableDaysPerMonth: Double = 22

// Calculate the daily rate given an hourly rate
func dailyRateFrom(hourlyRate: Int) -> Double {
  return Double(hourlyRate) * workHoursPerDay
}

// Calculate the monthly rate, given an hourly rate and a discount
func monthlyRateFrom(hourlyRate: Int, withDiscount discount: Double) -> Double {
  let discountAdjustment: Double = 1.0 - discount / 100
  let monthlyRate: Double = dailyRateFrom(hourlyRate: hourlyRate) * billableDaysPerMonth
  return (monthlyRate * discountAdjustment).rounded()
}

// Calculate the number of workdays given a budget, hourly rate and discount
func workdaysIn(budget: Double, hourlyRate: Int, withDiscount discount: Double) -> Double {
  let discountAdjustment: Double = 1.0 - discount / 100
  let dailyRate: Double = dailyRateFrom(hourlyRate: hourlyRate)
  return (budget / (dailyRate * discountAdjustment)).rounded(.down)
}
