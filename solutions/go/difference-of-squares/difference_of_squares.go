package diffsquares

func SquareOfSum(n int) int {
	sum := (n + n*n) / 2
	return sum * sum
}

func SumOfSquares(n int) int {
	return (n + 3*n*n + 2*n*n*n) / 6
}

func Difference(n int) int {
	return SquareOfSum(n) - SumOfSquares(n)
}
