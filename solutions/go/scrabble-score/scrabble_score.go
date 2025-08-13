package scrabble

import "strings"

// Score returns the Scrabble score for a given word.
func Score(word string) (sum int) {
	word = strings.ToUpper(word)
	for _, c := range word {
		sum += scoreLetter(c)
	}
	return
}

func scoreLetter(char rune) int {
	switch char {
	case 'A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T':
		return 1
	case 'D', 'G':
		return 2
	case 'B', 'C', 'M', 'P':
		return 3
	case 'F', 'H', 'V', 'W', 'Y':
		return 4
	case 'K':
		return 5
	case 'J', 'X':
		return 8
	case 'Q', 'Z':
		return 10
	default:
		return 0
	}
}
