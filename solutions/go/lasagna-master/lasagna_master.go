package lasagna

const defaultAverageLayerTime = 2
const noodlesPerLayer = 50
const saucePerLayer = 0.2

// PreparationTime return the estimate for the total preparation time based
// on the number of layers and the average preparation time per layer.
func PreparationTime(lasagnaLayers []string, averageLayerTime int) int {
	if averageLayerTime == 0 {
		averageLayerTime = defaultAverageLayerTime
	}
	return averageLayerTime * len(lasagnaLayers)
}

// Quantities returns the quantity of noodles and sauce needed to make your
// lasagna based on a slice of layers given as the parameter.
func Quantities(lasagnaLayers []string) (noodles int, sauce float64) {
	noodleLayers := 0
	sauceLayers := 0.0
	for i := 0; i < len(lasagnaLayers); i++ {
		switch lasagnaLayers[i] {
		case "noodles":
			noodleLayers++
		case "sauce":
			sauceLayers++
		}
	}
	noodles = noodlesPerLayer * noodleLayers
	sauce = saucePerLayer * sauceLayers
	return
}

// AddSecretIngredient replaces the last item in the seconds list with the
// secret ingredient based on two slices of ingredients; first parameter is
// the list with the secret ingredient, the second is the list without it.
func AddSecretIngredient(withSecret, withoutSecret []string) {
	withoutSecret[len(withoutSecret)-1] = withSecret[len(withSecret)-1]
}

// ScaleRecipe returns a slice of amounts for the desired number of portions.
func ScaleRecipe(baseQuantities []float64, desiredPortions int) []float64 {
	scaledQuantities := make([]float64, len(baseQuantities))

	for i := 0; i < len(scaledQuantities); i++ {
		scaledQuantities[i] = baseQuantities[i] / 2.0 * float64(desiredPortions)
	}
	return scaledQuantities
}
