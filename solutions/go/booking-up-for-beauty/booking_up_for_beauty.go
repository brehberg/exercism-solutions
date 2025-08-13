package booking

import (
	"fmt"
	"time"
)

const openingDate = "September 15th in 2012"
const openingFormat = "January 02th in 2006"

const scheduleFormat = "1/2/2006 15:04:05"
const checkerFormat = "January 2, 2006 15:04:05"
const appointmentFormat = "Monday, January 2, 2006 15:04:05"
const outputFormat = "Monday, January 2, 2006, at 15:04"

// Schedule returns a time.Time from a string containing a date.
func Schedule(date string) time.Time {
	scheduleTime, _ := time.Parse(scheduleFormat, date)
	return scheduleTime
}

// HasPassed returns whether a date has passed.
func HasPassed(date string) bool {
	dateTime, _ := time.Parse(checkerFormat, date)
	return dateTime.Before(time.Now())
}

// IsAfternoonAppointment returns whether a time is in the afternoon.
func IsAfternoonAppointment(date string) bool {
	appointmentTime, _ := time.Parse(appointmentFormat, date)
	hour := appointmentTime.Hour()
	return 12 <= hour && hour < 18
}

// Description returns a formatted string of the appointment time.
func Description(date string) string {
	formattedTime := Schedule(date).Format(outputFormat)
	return fmt.Sprintf("You have an appointment on %s.", formattedTime)
}

// AnniversaryDate returns a Time with this year's anniversary.
func AnniversaryDate() time.Time {
	anniversary, _ := time.Parse(openingFormat, openingDate)
	additionalYears := time.Now().Year() - anniversary.Year()
	return anniversary.AddDate(additionalYears, 0, 0)
}
