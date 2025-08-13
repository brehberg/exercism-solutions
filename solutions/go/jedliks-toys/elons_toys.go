package elon

import "fmt"

const (
	distanceMsg = "Driven %d meters"
	batteryMsg  = "Battery at %d%%"
)

// Drive updates the number of meters driven based on the car's speed,
// and reduces the battery according to the battery drainage.
func (c *Car) Drive() {
	if c.battery >= c.batteryDrain {
		c.distance += c.speed
		c.battery -= c.batteryDrain
	}
}

// DisplayDistance returns the distance as displayed on the LED.
func (c Car) DisplayDistance() string {
	return fmt.Sprintf(distanceMsg, c.distance)
}

// DisplayBattery returns the battery percentage as displayed on the LED.
func (c Car) DisplayBattery() string {
	return fmt.Sprintf(batteryMsg, c.battery)
}

// CanFinish returns true if the car can finish the race; otherwise, false.
func (c Car) CanFinish(trackDistance int) bool {
	return c.battery/c.batteryDrain*c.speed >= trackDistance
}
