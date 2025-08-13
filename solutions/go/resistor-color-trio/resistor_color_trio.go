package resistorcolortrio

import (
	"fmt"
	"math"
)

// Better Be Right Or Your Great Big Values Go Wrong
var values = [10]string{
	"black", "brown", "red", "orange", "yellow",
	"green", "blue", "violet", "grey", "white"}

// Label describes the resistance value given the colors of a resistor.
// The label is a string with a resistance value with an unit appended
// (e.g. "33 ohms", "470 kiloohms").
func Label(colors []string) string {
	r := value(colors)
	switch {
	case r < 1000:
		return fmt.Sprintf("%d ohms", r)
	case r < 1000_000:
		return fmt.Sprintf("%d kiloohms", r/1000)
	case r < 1000_000_000:
		return fmt.Sprintf("%d megaohms", r/1000_000)
	default:
		return fmt.Sprintf("%d gigaohms", r/1000_000_000)
	}
}

// Value should return the resistance value of a resistor with a given colors.
func value(colors []string) int {
	if len(colors) < 3 {
		return -1
	}

	var result int
	var exponent float64

	for i, value := range values {
		if colors[0] == value {
			result += i * 10
		}
		if colors[1] == value {
			result += i
		}
		if colors[2] == value {
			exponent = float64(i)
		}
	}
	return result * int(math.Pow(10.0, exponent))
}
