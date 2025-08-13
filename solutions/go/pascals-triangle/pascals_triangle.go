package pascal

// Triange returns Pascal's triangle up to a given number of rows.
func Triangle(n int) (triange [][]int) {
	row := []int{1}
	for n > 0 {
		triange = append(triange, row)
		row = generateNextRow(row)
		n--
	}
	return
}

func generateNextRow(previous []int) []int {
	row := []int{1}
	for i := 1; i < len(previous); i++ {
		row = append(row, previous[i-1]+previous[i])
	}
	row = append(row, 1)
	return row
}
