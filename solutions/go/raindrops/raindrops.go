package raindrops

import "strconv"

// Corresponding bit will be set when value is a factor of the given number.
type factor struct {
	value int
	bit   int8
}

var factors = []factor{{3, 0b1}, {5, 0b10}, {7, 0b100}}
var raindrops = []string{"", "Pling", "Plang", "PlingPlang",
	"Plong", "PlingPlong", "PlangPlong", "PlingPlangPlong"}

// Convert returns a string that contains raindrop sounds corresponding to potential factors.
// The rules of raindrops are that if a given number:
//
//	has 3 as a factor, add 'Pling' to the result.
//	has 5 as a factor, add 'Plang' to the result.
//	has 7 as a factor, add 'Plong' to the result.
//	does not have any of 3, 5, or 7 as a factor, result should be the digits of the number.
func Convert(number int) string {
	var index int8
	for _, rain := range factors {
		index += divisible_by(number, rain)
	}
	raindrops[0] = strconv.Itoa(number)
	return raindrops[index]
}

// divisible_by tests if rain value is a factor of number by using the modulo operation.
func divisible_by(number int, rain factor) int8 {
	if number%rain.value != 0 {
		return 0
	}
	return rain.bit
}
