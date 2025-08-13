package resistorcolortrio

import (
	"fmt"
	"math"
)

// Better Be Right Or Your Great Big Values Go Wrong
var values = map[string]int{
	"black": 0, "brown": 1, "red": 2, "orange": 3, "yellow": 4,
	"green": 5, "blue": 6, "violet": 7, "grey": 8, "white": 9}

var labels = []struct {
	val  int
	text string
}{{1e9, "giga"}, {1e6, "mega"}, {1e3, "kilo"}}

// Label describes the resistance value given the colors of a resistor.
// The label is a string with a resistance value with an unit appended
// (e.g. "33 ohms", "470 kiloohms").
func Label(colors []string) string {
	r := value(colors)
	for _, label := range labels {
		if r >= label.val {
			return fmt.Sprintf("%d %vohms", r/label.val, label.text)
		}
	}
	return fmt.Sprintf("%d ohms", r)
}

// Value should return the resistance value of a resistor with a given colors.
func value(colors []string) int {
	if len(colors) < 3 {
		return -1
	}
	base := values[colors[0]]*10 + values[colors[1]]
	exp := float64(values[colors[2]])
	return base * int(math.Pow(10, exp))
}
