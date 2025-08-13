package allyourbase

import (
	"errors"
	"math"
	"slices"
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
	offset := len(inputDigits) - 1
	for i, d := range inputDigits {
		if d < 0 || d >= inputBase {
			return nil, ErrInputDigits
		}
		value += d * int(math.Pow(float64(inputBase), float64(offset-i)))
	}

	// convert whole integer value to sequence of digits in output base
	var outputDigits []int
	for value >= outputBase {
		outputDigits = slices.Insert(outputDigits, 0, value%outputBase)
		value /= outputBase
	}
	return slices.Insert(outputDigits, 0, value), nil
}
