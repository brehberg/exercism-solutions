// Package bob provides pretty limited responses when people talk to him.
//
// Bob is a lackadaisical teenager. He likes to think that he's very cool.
// And he definitely doesn't get excited about things. That wouldn't be cool.
package bob

import "strings"

// Hey returns a string that only ever answers one of five things.
func Hey(remark string) string {
	remark = strings.Trim(remark, " \t\n\r")

	switch {
	case is_silence(remark):
		return "Fine. Be that way!"
	case is_yelling_question(remark):
		return "Calm down, I know what I'm doing!"
	case is_yelling(remark):
		return "Whoa, chill out!"
	case is_question(remark):
		return "Sure."
	default:
		return "Whatever."
	}
}

func is_yelling(str string) bool {
	return str == strings.ToUpper(str) && str != strings.ToLower(str)
}

func is_question(str string) bool {
	return str[len(str)-1] == '?'
}

func is_yelling_question(str string) bool {
	return is_yelling(str) && is_question(str)
}

func is_silence(str string) bool {
	return str == ""
}
