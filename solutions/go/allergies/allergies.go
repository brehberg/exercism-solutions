package allergies

import "slices"

var allergens = [8]string{
	"eggs", "peanuts", "shellfish", "strawberries",
	"tomatoes", "chocolate", "pollen", "cats"}

// Allergies returns the whole list of allergic allergens.
func Allergies(allergies uint) []string {
	result := []string{}
	for i, allergen := range allergens {
		if allergies&(1<<i) > 0 {
			result = append(result, allergen)
		}
	}
	return result
}

// AllergicTo return true if allergic to the given allergen.
func AllergicTo(allergies uint, allergen string) bool {
	return slices.Contains(Allergies(allergies), allergen)
}
