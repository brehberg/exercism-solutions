package anagram

import (
	"sort"
	"strings"
)

func sorted(word string) string {
	chars := []rune(word)
	sort.Slice(chars, func(i, j int) bool {
		return chars[i] < chars[j]
	})
	return string(chars)
}

// Detect returns all candidates that are anagrams of, but not equal to, 'subject'.
func Detect(subject string, candidates []string) (anagrams []string) {
	base := strings.ToLower(subject)
	base_letters := sorted(base)

	for _, candidate := range candidates {
		if len(base) != len(candidate) {
			continue
		}
		word := strings.ToLower(candidate)
		if base == word {
			continue
		}
		word_letters := sorted(word)
		if base_letters == word_letters {
			anagrams = append(anagrams, candidate)
		}
	}
	return
}
