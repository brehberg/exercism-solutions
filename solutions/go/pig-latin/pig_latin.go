package piglatin

import (
	"strings"
)

var vowels = map[byte]bool{'a': true, 'e': true, 'i': true, 'o': true, 'u': true}
var specials = map[string]bool{"xr": true, "yt": true}
var vowels_y = map[byte]bool{'a': true, 'e': true, 'i': true, 'o': true, 'u': true, 'y': true}

func Sentence(phrase string) string {
	result := strings.Builder{}

	for _, word := range strings.Split(phrase, " ") {
		if result.Len() != 0 {
			result.WriteByte(' ')
		}

		// If a word begins with a vowel, or starts with "xr" or "yt",
		// add an "ay" sound to the end of the word.
		if vowels[word[0]] || specials[word[:2]] {
			result.WriteString(word + "ay")
			continue
		}

		for pos := 1; pos < len(word); pos++ {
			letter := word[pos]
			// If a word starts with one or more consonants followed by "y",
			// first move the consonants preceding the "y"to the end of the word,
			// and then add an "ay" sound to the end of the word.
			if vowels_y[letter] {

				// If a word starts with zero or more consonants followed by "qu",
				// first move those consonants (if any) and the "qu" part to the end
				// of the word, and then add an "ay" sound to the end of the word.
				if letter == 'u' && word[pos-1] == 'q' {
					pos++
				}

				// If a word begins with one or more consonants, first move those consonants
				// to the end of the word and then add an "ay" sound to the end of the word.
				result.WriteString(word[pos:] + word[:pos] + "ay")
				break
			}
		}
	}
	return result.String()
}
