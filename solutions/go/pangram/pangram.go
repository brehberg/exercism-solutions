package pangram

import "strings"

// IsPangram returns true if a word or sentence is a pangram. A pangram
// is a sentence using every letter of the alphabet at least once.
func IsPangram(input string) bool {
	letters := make(map[rune]bool)
	for _, letter := range strings.ToLower(input) {
		letters[letter] = true
	}

	for char := 'a'; char < 'z'; char++ {
		if !letters[char] {
			return false
		}
	}
	return true
}
