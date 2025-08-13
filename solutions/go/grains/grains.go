// Package grains calculate the number of grains of wheat on a chessboard given
// that the number on each square doubles. (where square 1 has one grain, square
// 2 has two grains, and so on). There are 64 squares on a chessboard.
package grains

import "errors"

var ErrInvalidNumber = errors.New("square must be between 1 and 64 (inclusive)")

// Square returns how many grains were on a given square
func Square(number int) (start uint64, e error) {
	if number < 1 || number > 64 {
		e = ErrInvalidNumber
		return
	}
	start = 1
	return start << (number - 1), nil
}

// Total returns the total number of grains on the chessboard
func Total() (total uint64) {
	return total - 1
}
