package space

type Planet string

func Age(seconds float64, planet Planet) float64 {
	years_on_earth := seconds / 31557600
	switch planet {
	case "Earth":
		return years_on_earth
	case "Mercury":
		return years_on_earth / 0.2408467
	case "Venus":
		return years_on_earth / 0.61519726
	case "Mars":
		return years_on_earth / 1.8808158
	case "Jupiter":
		return years_on_earth / 11.862615
	case "Saturn":
		return years_on_earth / 29.447498
	case "Uranus":
		return years_on_earth / 84.016846
	case "Neptune":
		return years_on_earth / 164.79132
	default:
		return -1
	}
}
