package resistorcolor

// Better Be Right Or Your Great Big Values Go Wrong
var values = [10]string{
	"black", "brown", "red", "orange", "yellow",
	"green", "blue", "violet", "grey", "white"}

// Colors should return the list of all colors.
func Colors() []string {
	return values[:]
}

// ColorCode returns the resistance value of the given color.
func ColorCode(color string) int {
	for i, value := range values {
		if color == value {
			return i
		}
	}
	return -1
}
