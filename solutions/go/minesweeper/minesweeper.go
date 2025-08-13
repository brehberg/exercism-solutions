package minesweeper

const mine = '*'

// Annotate returns an annotated board
func Annotate(board []string) []string {
	// check for no rows or columns
	rows := len(board)
	if rows == 0 {
		return board
	}
	cols := len(board[0])
	if cols == 0 {
		return board
	}

	// function to count neighboring mines for a given row
	checkRow := func(row int, col int) (count int) {
		if row < 0 || row >= rows {
			return
		}
		count += boolToInt(board[row][col] == mine)
		count += boolToInt(col-1 >= 0 && board[row][col-1] == mine)
		count += boolToInt(col+1 < cols && board[row][col+1] == mine)
		return
	}

	result := board
	// Traverse the board to find empty space and count mines around it
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if board[r][c] == mine {
				continue // skip if cell is a mine
			}

			mines := checkRow(r, c)
			mines += checkRow(r-1, c)
			mines += checkRow(r+1, c)

			// replace rune in this row's string with the number of mines
			if mines > 0 {
				result[r] = result[r][:c] + string(rune(int('0')+mines)) + result[r][c+1:]
			}
		}
	}
	return result
}

func boolToInt(b bool) int {
	if b {
		return 1
	}
	return 0
}
