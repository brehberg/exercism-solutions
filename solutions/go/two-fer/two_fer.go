// This is a "stub" file.  It's a little start on your solution.
// It's not a complete solution though; you have to write some code.

// Package twofer determines what you will say as you give away an extra cookie.
package twofer

import "fmt"

const twoFerMsg = "One for %s, one for me."

// ShareWith returns the response with "you" as the default of no name is given
func ShareWith(name string) string {
	you := "you"
	if name != "" {
		you = name
	}
	return fmt.Sprintf(twoFerMsg, you)
}
