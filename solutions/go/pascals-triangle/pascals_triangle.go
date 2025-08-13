package pascal

// Triange returns Pascal's triangle up to a given number of rows.
func Triangle(n int) [][]int {
	triange := make([][]int, n)
	row := []int{1}
	for i := 0; i < n; i++ {
		triange[i] = row
		row = generateNextRow(row)
	}
	return triange
}

func generateNextRow(previous []int) []int {
	row := make([]int, len(previous)+1)
	row[0], row[len(previous)] = 1, 1
	for i := 1; i < len(previous); i++ {
		row[i] = previous[i-1] + previous[i]
	}
	return row
}
