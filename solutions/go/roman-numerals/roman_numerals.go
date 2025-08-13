package romannumerals

import (
	"errors"
	"strings"
)

type mapping struct {
	symbols     string
	replacement string
}

// define mapping from a set of symbols to its replacement
var replacements = []mapping{
	{"IIIII", "V"}, // 5
	{"IIII", "IV"}, // 4
	{"VV", "X"},    // 10
	{"VIV", "IX"},  // 9
	{"XXXXX", "L"}, // 50
	{"XXXX", "XL"}, // 40
	{"LL", "C"},    // 100
	{"LXL", "XC"},  // 90
	{"CCCCC", "D"}, // 500
	{"CCCC", "CD"}, // 400
	{"DD", "M"},    // 1000
	{"DCD", "CM"},  // 900
}

// toRomanNumeral returns a string containing the Roman Numeral representation
// for a positive integer between 1 and 3999 (both included)
func ToRomanNumeral(input int) (string, error) {
	if input < 1 || input > 3999 {
		return "", errors.New("invalid input")
	}

	output := strings.Repeat("I", input)
	for _, m := range replacements {
		output = strings.Replace(output, m.symbols, m.replacement, -1)
	}
	return output, nil
}
