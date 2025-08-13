// Package clock implement a clock that handles times without dates.
package clock

import "fmt"

const (
	minsPerHour = 60
	minsPerDay  = 1440
)

// Clock type tracks time in hours and minutes.
type Clock struct {
	hour int
	min  int
}

func normalize(n, factor int) int {
	return (n%factor + factor) % factor
}

// New returns a clock value with initial hours and minutes set.
func New(h, m int) Clock {
	minutes := normalize(h*minsPerHour+m, minsPerDay)

	return Clock{
		hour: minutes / minsPerHour,
		min:  minutes % minsPerHour}
}

// Add returns a clock value with minutes added to it.
func (c Clock) Add(m int) Clock {
	return New(c.hour, c.min+m)
}

// Subtract returns a clock value with minutes subtracted from it.
func (c Clock) Subtract(m int) Clock {
	return New(c.hour, c.min-m)
}

// String returns the clock time in HH:MM format.
func (c Clock) String() string {
	return fmt.Sprintf("%02d:%02d", c.hour, c.min)
}
