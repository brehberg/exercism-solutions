package allyourbase

import (
	"errors"
)

var ErrInputBase = errors.New("input base must be >= 2")
var ErrOutputBase = errors.New("output base must be >= 2")
var ErrInputDigits = errors.New("all digits must satisfy 0 <= d < input base")

func ConvertToBase(inputBase int, inputDigits []int, outputBase int) ([]int, error) {
	if inputBase < 2 {
		return nil, ErrInputBase
	}
	if outputBase < 2 {
		return nil, ErrOutputBase
	}

	// convert sequence of digits in input base to whole integer value
	value := 0
	for _, digit := range inputDigits {
		if digit < 0 || digit >= inputBase {
			return nil, ErrInputDigits
		}
		value *= inputBase
		value += digit
	}

	// convert whole integer value to sequence of digits in output base
	var outputDigits []int
	for value >= outputBase {
		outputDigits = append([]int{value % outputBase}, outputDigits...)
		value /= outputBase
	}
	return append([]int{value}, outputDigits...), nil
}
