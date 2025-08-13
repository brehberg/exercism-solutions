// Package triangle determines if a triangle is equilateral, isosceles, or scalene.
package triangle

// Type Kind is returned by function KindFromSides.
type Kind int

const (
	NaT = 0 // not a triangle
	Equ = 1 // equilateral
	Iso = 2 // isosceles
	Sca = 3 // scalene
)

// KindFromSides return the kind of triangle of a triangle with 'a', 'b' and 'c' as lengths.
func KindFromSides(a, b, c float64) (k Kind) {
	// all side lenghts must be positive
	if a <= 0 || b <= 0 || c <= 0 {
		return NaT
	}

	// side lengths cannot violate triangle inequality
	if a+b < c || b+c < a || a+c < b {
		return NaT
	}

	switch {
	case a == b && b == c:
		k = Equ
	case a == b, b == c, a == c:
		k = Iso
	default:
		k = Sca
	}
	return
}
