package clock

import (
	"fmt"
	"time"
)

// Define the Clock type here.
type Clock struct {
	hour int
	min  int
}

func New(h, m int) Clock {
	initial := time.Date(2023, 3, 25, h, m, 0, 0, time.UTC)
	return Clock{hour: initial.Hour(), min: initial.Minute()}
}

func (c Clock) Add(m int) Clock {
	return New(c.hour, c.min+m)
}

func (c Clock) Subtract(m int) Clock {
	return New(c.hour, c.min-m)
}

func (c Clock) String() string {
	return fmt.Sprintf("%02d:%02d", c.hour, c.min)
}
