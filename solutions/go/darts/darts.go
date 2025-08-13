package darts

import "math"

func Score(x, y float64) int {
	d := distance(x, y)
	switch {
	case d <= 1:
		return 10
	case d <= 5:
		return 5
	case d <= 10:
		return 1
	default:
		return 0
	}
}

func distance(x, y float64) float64 {
	return math.Sqrt(x*x + y*y)
}
