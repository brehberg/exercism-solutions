package raindrops

import "strconv"

// Provides the mapping of factors to their corresponding rain sounds.
var mapping = []struct {
	factor int
	sound  string
}{{3, "Pling"}, {5, "Plang"}, {7, "Plong"}}

// Convert returns a string that contains raindrop sounds corresponding to potential factors.
// The rules of raindrops are that if a given number:
//
//	has 3 as a factor, add 'Pling' to the result.
//	has 5 as a factor, add 'Plang' to the result.
//	has 7 as a factor, add 'Plong' to the result.
//	does not have any of 3, 5, or 7 as a factor, result should be the digits of the number.
func Convert(number int) (result string) {
	for _, rain := range mapping {
		if divisible_by(rain.factor, number) {
			result += rain.sound
		}
	}
	if result == "" {
		result = strconv.Itoa(number)
	}
	return
}

// divisible_by tests if one number is a factor of another by using the modulo operation.
func divisible_by(n, number int) bool {
	return number%n == 0
}
