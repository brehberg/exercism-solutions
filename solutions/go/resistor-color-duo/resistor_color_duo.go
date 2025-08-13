package resistorcolorduo

// Better Be Right Or Your Great Big Values Go Wrong
var values = [10]string{
	"black", "brown", "red", "orange", "yellow",
	"green", "blue", "violet", "grey", "white"}

// Value should return the resistance value of a resistor with a given colors.
func Value(colors []string) (result int) {
	if len(colors) < 2 {
		return -1
	}

	for i, value := range values {
		if colors[0] == value {
			result += i * 10
		}
		if colors[1] == value {
			result += i
		}
	}
	return
}
