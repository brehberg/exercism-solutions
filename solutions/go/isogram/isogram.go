package isogram

import "strings"

// IsIsogram returns if a word or phrase is an isogram.
func IsIsogram(word string) bool {
	cleanWord := clean(word)
	for i, char := range cleanWord {
		if strings.ContainsRune(cleanWord[i+1:], char) {
			return false
		}
	}
	return true
}

// clean removes spaces and hyphens from word and converts to lowercase
func clean(word string) string {
	return strings.ToLower(strings.NewReplacer(" ", "", "-", "").Replace(word))
}
