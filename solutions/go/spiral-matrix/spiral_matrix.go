package spiralmatrix

type matrix [][]int

type value struct {
	prev int
	done int
}

type position struct {
	row int
	col int
	min int
	max int
}

func SpiralMatrix(size int) [][]int {
	spiral := make([][]int, size)
	for i := range spiral {
		spiral[i] = make([]int, size)
	}

	val := value{prev: 0, done: size * size}
	pos := position{row: 0, col: 0, min: -1, max: size}

	return go_right(spiral, val, pos)
}

func go_right(spiral matrix, val value, pos position) matrix {
	if val.prev == val.done {
		return spiral
	}
	if pos.col == pos.max {
		// position beyond last column, turn down instead
		pos.row += 1
		pos.col -= 1
		return go_down(spiral, val, pos)
	}
	// update this position and move right to next column
	spiral[pos.row][pos.col] = val.prev + 1
	val.prev += 1
	pos.col += 1
	return go_right(spiral, val, pos)
}

func go_down(spiral matrix, val value, pos position) matrix {
	if val.prev == val.done {
		return spiral
	}
	if pos.row == pos.max {
		// position beyond last row, turn left instead
		pos.row -= 1
		pos.col -= 1
		return go_left(spiral, val, pos)
	}
	// update this position and move down to next row
	spiral[pos.row][pos.col] = val.prev + 1
	val.prev += 1
	pos.row += 1
	return go_down(spiral, val, pos)
}

func go_left(spiral matrix, val value, pos position) matrix {
	if val.prev == val.done {
		return spiral
	}
	if pos.col == pos.min {
		// position beyond first column, turn up and increase min position
		pos.row -= 1
		pos.col += 1
		pos.min += 1
		return go_up(spiral, val, pos)
	}
	// update this position and move left to previous column
	spiral[pos.row][pos.col] = val.prev + 1
	val.prev += 1
	pos.col -= 1
	return go_left(spiral, val, pos)
}

func go_up(spiral matrix, val value, pos position) matrix {
	if val.prev == val.done {
		return spiral
	}
	if pos.row == pos.min {
		// position beyond first row, turn right and decrease max position
		pos.row += 1
		pos.col += 1
		pos.max -= 1
		return go_right(spiral, val, pos)
	}
	// update this position and move up to previous row
	spiral[pos.row][pos.col] = val.prev + 1
	val.prev += 1
	pos.row -= 1
	return go_up(spiral, val, pos)
}
