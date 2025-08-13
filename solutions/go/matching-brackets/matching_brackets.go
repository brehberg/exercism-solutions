package brackets

// mapping of each opening bracket to its corresponding closing bracket
var matches = map[rune]rune{
	'[': ']',
	'{': '}',
	'(': ')',
}

// Bracket checks that all the brackets and braces in the string are matched correctly,
// and nested correctly
func Bracket(input string) bool {
	closerNeeded := []rune{}

	// determine if matching bracket map contains the specified char value
	has_value := func(char rune) bool {
		for _, value := range matches {
			if value == char {
				return true
			}
		}
		return false
	}

	for _, c := range input {
		if closer, ok := matches[c]; ok {
			// opening bracket was found, add matching closing value to the stack
			closerNeeded = append(closerNeeded, closer)
		} else if has_value(c) {
			// closing bracket was found, is it the next expected value on stack?
			last := len(closerNeeded) - 1
			if len(closerNeeded) == 0 || closerNeeded[last] != c {
				return false
			}
			closerNeeded = closerNeeded[:last]
		}
	}

	return len(closerNeeded) == 0
}
