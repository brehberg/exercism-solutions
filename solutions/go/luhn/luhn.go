// Package luhn implements a simple checksum formula used to validate id numbers.
package luhn

import (
	"strconv"
	"strings"
)

// Valid returns true if the given number is valid via the luhn formula.
func Valid(id string) bool {
	id = strings.ReplaceAll(id, " ", "")
	return len(id) > 1 && check_digits(id)
}

func check_digits(id string) bool {
	sum := 0
	for i := 1; i <= len(id); i++ {
		n, err := strconv.Atoi(string(id[len(id)-i]))

		if err != nil {
			return false
		} else if i%2 != 0 {
			sum += n
		} else if n < 5 {
			sum += n * 2
		} else {
			sum += n*2 - 9
		}
	}
	return sum%10 == 0
}
