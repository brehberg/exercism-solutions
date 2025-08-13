package reverse

// Reverse returns the string with runes in reverse order
func Reverse(input string) (output string) {
	for _, char := range input {
		output = string(char) + output
	}
	return
}
