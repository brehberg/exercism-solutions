module main

// is_leap_year returns true if the given year
// is a leap year in the Gregorian calendar
fn is_leap_year(year int) bool {
	is_divisible_by := fn [year](n int) bool { 
		return year % n == 0
	}
	return is_divisible_by(4) &&
		(!is_divisible_by(100) || 
		is_divisible_by(400))
}
