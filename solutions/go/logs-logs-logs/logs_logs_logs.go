package logs

import "unicode/utf8"

var appIdentifiers = map[rune]string{
	'❗': "recommendation",
	'🔍': "search",
	'☀': "weather",
}

// Application identifies the application emitting the given log.
func Application(log string) string {
	for _, character := range log {
		app, known := appIdentifiers[character]
		if known {
			return app
		}
	}
	return "default"
}

// Replace replaces all occurrences of old with new, returning the modified log
// to the caller.
func Replace(log string, oldRune, newRune rune) string {
	modified := ""
	for _, character := range log {
		if character == oldRune {
			modified += string(newRune)
		} else {
			modified += string(character)
		}
	}
	return modified
}

// WithinLimit determines whether or not the number of characters in log is
// within the limit.
func WithinLimit(log string, limit int) bool {
	return utf8.RuneCountInString(log) <= limit
}
