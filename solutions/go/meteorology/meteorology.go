package meteorology

import "fmt"

// TemperatureUnit will be either Celsius or Fahrenheit.
type TemperatureUnit int

const (
	Celsius    TemperatureUnit = 0
	Fahrenheit TemperatureUnit = 1
)

func (unit TemperatureUnit) String() string {
	labels := []string{"°C", "°F"}
	return labels[unit]
}

// Temperature values consist of an integer and a temperature unit.
type Temperature struct {
	degree int
	unit   TemperatureUnit
}

func (temp Temperature) String() string {
	return fmt.Sprintf("%v %v", temp.degree, temp.unit)
}

// SpeedUnit will be either KmPerHour or MilesPerHour.
type SpeedUnit int

const (
	KmPerHour    SpeedUnit = 0
	MilesPerHour SpeedUnit = 1
)

func (unit SpeedUnit) String() string {
	labels := []string{"km/h", "mph"}
	return labels[unit]
}

// Speed of wind values consist of an integer and a speed unit.
type Speed struct {
	magnitude int
	unit      SpeedUnit
}

func (wind Speed) String() string {
	return fmt.Sprintf("%v %v", wind.magnitude, wind.unit)
}

// Meteorological data specifies location, temperature, wind direction, wind speed and humidity.
type MeteorologyData struct {
	location      string
	temperature   Temperature
	windDirection string
	windSpeed     Speed
	humidity      int
}

func (d MeteorologyData) String() string {
	const msg = "%v: %v, Wind %v at %v, %v%% Humidity"
	return fmt.Sprintf(msg, d.location, d.temperature, d.windDirection, d.windSpeed, d.humidity)
}
