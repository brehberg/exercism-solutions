package anagram

import (
	"reflect"
	"sort"
	"strings"
)

// Detect returns all candidates that are anagrams of, but not equal to, 'subject'.
func Detect(subject string, candidates []string) []string {
	base := strings.ToLower(subject)

	base_letters := strings.Split(base, "")
	sort.Strings(base_letters)

	var anagrams []string

	for _, candidate := range candidates {
		word := strings.ToLower(candidate)
		word_letters := strings.Split(word, "")
		sort.Strings(word_letters)

		if base != word && reflect.DeepEqual(base_letters, word_letters) {
			anagrams = append(anagrams, candidate)
		}
	}
	return anagrams
}
