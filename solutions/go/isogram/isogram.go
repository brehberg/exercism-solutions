package isogram

import (
	"strings"
	"unicode"
)

// IsIsogram returns if a word or phrase is an isogram.
func IsIsogram(word string) bool {
	cleanWord := strings.ToLower(word)
	for i, char := range cleanWord {
		if !unicode.IsLetter(char) {
			continue
		}
		if strings.ContainsRune(cleanWord[i+1:], char) {
			return false
		}
	}
	return true
}
