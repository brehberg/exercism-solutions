// Package weather provides tools for weather forecast in the country of Goblinocus.
package weather

// CurrentCondition represents the current weather condition.
var CurrentCondition string

// CurrentLocation represents the current weather location.
var CurrentLocation string

// Forecast returns a formatted string based on the given location and condition.
func Forecast(city, condition string) string {
	CurrentLocation, CurrentCondition = city, condition
	return CurrentLocation + " - current weather condition: " + CurrentCondition
}
