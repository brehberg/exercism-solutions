package spiralmatrix

// sequence of directions to fill the matrix in clockwise order
type direction int

const (
	right direction = iota
	down
	left
	up
)

// row and column indexes representing a position in the matrix
type position struct {
	row, col int
}

// SpiralMatrix return a square matrix of a given size that is filled with natural numbers,
// starting from 1 in the top-left corner, increasing in an inward, clockwise spiral order.
func SpiralMatrix(size int) [][]int {
	spiral := make([][]int, size)
	for i := range spiral {
		spiral[i] = make([]int, size)
	}

	pos := position{row: 0, col: 0}
	dir := right

	for val := 1; val <= size*size; val++ {
		// update value at this position and check for valid next move
		spiral[pos.row][pos.col] = val
		next := adjustPosition(pos, dir)

		if next.col >= size || // position beyond last column, turn down instead
			next.row >= size || // position beyond last row, turn left instead
			next.col < 0 || // position beyond first column, turn up instead
			next.row < 0 || // position beyond first row, turn right instead
			spiral[next.row][next.col] > 0 { // position filled, turn instead
			dir = (dir + 1) % 4
		}
		pos = adjustPosition(pos, dir)
	}
	return spiral
}

// adjustPosition return a new position based on the current direction
func adjustPosition(pos position, dir direction) position {
	switch dir {
	case right:
		pos.col += 1 // move right to next column
	case down:
		pos.row += 1 // move down to next row
	case left:
		pos.col -= 1 // move left to previous column
	case up:
		pos.row -= 1 // move up to previous row
	}
	return pos
}
