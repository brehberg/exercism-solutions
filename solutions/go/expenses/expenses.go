package expenses

// Record represents an expense record.
type Record struct {
	Day      int
	Amount   float64
	Category string
}

// DaysPeriod represents a period of days for expenses.
type DaysPeriod struct {
	From int
	To   int
}

// ErrorUnknownCategory returned when not a category of any of the records.
type ErrorUnknownCategory struct {
	category string
}

func (e ErrorUnknownCategory) Error() string {
	return "unknown category " + e.category
}

// Filter returns the records for which the predicate function returns true.
func Filter(in []Record, predicate func(Record) bool) (out []Record) {
	for _, record := range in {
		if predicate(record) {
			out = append(out, record)
		}
	}
	return
}

// ByDaysPeriod returns predicate function that returns true when
// the day of the record is inside the period of day and false otherwise.
func ByDaysPeriod(p DaysPeriod) func(Record) bool {
	return func(r Record) bool {
		return p.From <= r.Day && r.Day <= p.To
	}
}

// ByCategory returns predicate function that returns true when
// the category of the record is the same as the provided category
// and false otherwise.
func ByCategory(c string) func(Record) bool {
	return func(r Record) bool {
		return c == r.Category
	}
}

// TotalByPeriod returns total amount of expenses for records
// inside the period p.
func TotalByPeriod(in []Record, p DaysPeriod) float64 {
	var totalAmount float64
	for _, record := range Filter(in, ByDaysPeriod(p)) {
		totalAmount += record.Amount
	}
	return totalAmount
}

// CategoryExpenses returns total amount of expenses for records
// in category c that are also inside the period p.
// An error must be returned only if there are no records in the list that belong
// to the given category, regardless of period of time.
func CategoryExpenses(in []Record, p DaysPeriod, c string) (float64, error) {
	filteredByCategory := Filter(in, ByCategory(c))
	if len(filteredByCategory) < 1 {
		return 0, &ErrorUnknownCategory{category: c}
	}
	return TotalByPeriod(filteredByCategory, p), nil
}
