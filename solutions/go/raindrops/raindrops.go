package raindrops

import "strconv"

// Convert returns a string that contains raindrop sounds corresponding to potential factors.
// The rules of raindrops are that if a given number:
//
//	has 3 as a factor, add 'Pling' to the result.
//	has 5 as a factor, add 'Plang' to the result.
//	has 7 as a factor, add 'Plong' to the result.
//	does not have any of 3, 5, or 7 as a factor, result should be the digits of the number.
func Convert(number int) (result string) {
	if divisible_by(3, number) {
		result += "Pling"
	}
	if divisible_by(5, number) {
		result += "Plang"
	}
	if divisible_by(7, number) {
		result += "Plong"
	}
	if result == "" {
		result = strconv.Itoa(number)
	}
	return
}

func divisible_by(n, number int) bool {
	return number%n == 0
}
