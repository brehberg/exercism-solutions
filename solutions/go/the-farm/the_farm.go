package thefarm

// See types.go for the types defined for this exercise.

import (
	"errors"
	"fmt"
)

var errNegativeFodder = errors.New("negative fodder")
var errDivisionByZero = errors.New("division by zero")

// SillyNephewError should be returned in case the number of cows is negative.
type SillyNephewError struct {
	cows int
}

func (e *SillyNephewError) Error() string {
	return fmt.Sprintf("silly nephew, there cannot be %d cows", e.cows)
}

// DivideFood computes the fodder amount per cow for the given cows.
func DivideFood(weightFodder WeightFodder, cows int) (float64, error) {
	fodder, err := weightFodder.FodderAmount()
	if err == ErrScaleMalfunction {
		fodder *= 2
	} else if err != nil {
		return 0, err
	}

	if fodder < 0 {
		return 0, errNegativeFodder
	} else if cows == 0 {
		return 0, errDivisionByZero
	} else if cows < 0 {
		return 0, &SillyNephewError{cows: cows}
	}

	return fodder / float64(cows), nil
}
