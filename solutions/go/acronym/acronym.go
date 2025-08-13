// Acronym convert a phrase to its acronym.
package acronym

import (
	"strings"
	"unicode"
)

const (
	insideWord int = iota
	seekingWord
)

// Abbreviate returns the capitalized first letter of each word in phrase
func Abbreviate(phrase string) string {
	result := strings.Builder{}
	state := seekingWord

	for _, c := range phrase {
		switch {
		case state == seekingWord && unicode.IsLetter(c):
			result.WriteRune(unicode.ToUpper(c))
			state = insideWord
		case state == insideWord && !(unicode.IsLetter(c) || c == '\''):
			state = seekingWord
		}
	}

	return result.String()
}
