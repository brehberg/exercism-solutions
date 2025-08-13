// Package rotationalcipher implementats the rotational cipher,
// also sometimes called the Caesar cipher.
package rotationalcipher

// RotationalCipher returns a rotated string when given a plaintext and
// amount to shift by. Each letter is shifted for the value of the key.
func RotationalCipher(plain string, shiftKey int) string {
	output := []rune(plain)
	for i, char := range output {
		if char >= 'a' && char <= 'z' {
			output[i] = rotate(char, rune(shiftKey), 'a')
		} else if char >= 'A' && char <= 'Z' {
			output[i] = rotate(char, rune(shiftKey), 'A')
		}
	}
	return string(output)
}

func rotate(c, n, start rune) rune {
	return start + (c+n-start)%26
}
