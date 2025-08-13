// Package leap has functions for dealing with leap years.
package leap

// IsLeapYear returns if a given year is a leap year.
//
// Occurs on every year that is evenly divisible by 4
// except every year that is evenly divisible by 100
// unless the year is also evenly divisible by 400.
func IsLeapYear(year int) bool {
	isDivisibleBy := func(n int) bool { return year%n == 0 }
	return isDivisibleBy(4) && !isDivisibleBy(100) || isDivisibleBy(400)
}
