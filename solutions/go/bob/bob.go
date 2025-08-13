// Package bob provides pretty limited responses when people talk to him.
//
// Bob is a lackadaisical teenager. He likes to think that he's very cool.
// And he definitely doesn't get excited about things. That wouldn't be cool.
package bob

import "strings"

// Hey returns a string that only ever answers one of five things.
func Hey(remark string) string {
	remark = strings.TrimSpace(remark)

	if isSilence(remark) {
		return "Fine. Be that way!"
	}

	yelling := isYelling(remark)
	question := isQuestion(remark)

	switch {
	case yelling && question:
		return "Calm down, I know what I'm doing!"
	case yelling:
		return "Whoa, chill out!"
	case question:
		return "Sure."
	default:
		return "Whatever."
	}
}

func isYelling(str string) bool {
	return str == strings.ToUpper(str) && str != strings.ToLower(str)
}

func isQuestion(str string) bool {
	return strings.HasSuffix(str, "?")
}

func isSilence(str string) bool {
	return str == ""
}
