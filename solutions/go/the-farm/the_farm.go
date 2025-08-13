package thefarm

// See types.go for the types defined for this exercise.

import (
	"errors"
	"fmt"
)

var errNonPositiveCows = errors.New("invalid number of cows")

// InvalidCowsError should be returned in case the number of cows not positive.
type InvalidCowsError struct {
	cows int
}

func (e *InvalidCowsError) Error() string {
	if e.cows == 0 {
		return "0 cows are invalid: no cows don't need food"
	}
	return fmt.Sprintf("%d cows are invalid: there are no negative cows", e.cows)
}

// DivideFood computes the fodder amount per cow for the given cows.
func DivideFood(calulator FodderCalculator, cows int) (float64, error) {
	fodder, err := calulator.FodderAmount(cows)
	if err != nil {
		return 0, err
	}
	factor, err := calulator.FatteningFactor()
	if err != nil {
		return 0, err
	}
	return fodder * factor / float64(cows), nil
}

// ValidateInputAndDivideFood verifies the number of cows is positive and
// then computes the fodder amount per cow for the given cows.
func ValidateInputAndDivideFood(calulator FodderCalculator, cows int) (float64, error) {
	if cows <= 0 {
		return 0, errNonPositiveCows
	}
	return DivideFood(calulator, cows)
}

// ValidateNumberOfCows returns a custom error if the number of cows is invalid.
func ValidateNumberOfCows(cows int) error {
	if cows <= 0 {
		return &InvalidCowsError{cows: cows}
	}
	return nil
}
