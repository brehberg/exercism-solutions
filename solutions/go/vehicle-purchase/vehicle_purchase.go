package purchase

import "fmt"

// NeedsLicense determines whether a license is needed to drive a type of vehicle. Only "car" and "truck" require a license.
func NeedsLicense(kind string) bool {
	return kind == "car" || kind == "truck"
}

// ChooseVehicle recommends a vehicle for selection. It always recommends the vehicle that comes first in lexicographical order.
func ChooseVehicle(option1, option2 string) string {
	choice := "%s is clearly the better choice."
	if option1 < option2 {
		return fmt.Sprintf(choice, option1)
	} else {
		return fmt.Sprintf(choice, option2)
	}

}

const smallDiscount = .80
const mediumDiscount = .70
const largeDiscount = .50

// CalculateResellPrice calculates how much a vehicle can resell for at a certain age.
func CalculateResellPrice(originalPrice, age float64) float64 {
	if age < 3 {
		return originalPrice * smallDiscount
	} else if age >= 10 {
		return originalPrice * largeDiscount
	} else {
		return originalPrice * mediumDiscount
	}
}
