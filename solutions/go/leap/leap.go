// Package leap has functions for dealing with leap years.
package leap

// IsLeapYear returns if a given year is a leap year.
//
// Occurs on every year that is evenly divisible by 4
// except every year that is evenly divisible by 100
// unless the year is also evenly divisible by 400.
func IsLeapYear(year int) bool {
	if year%100 == 0 {
		return year%400 == 0
	} else {
		return year%4 == 0
	}
}
