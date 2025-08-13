package chessboard

// File stores if a square is occupied by a piece - this is a slice of bools.
type File [8]bool

// Chessboard contains a map of eight Files, accessed with keys from "A" to "H".
type Chessboard map[string]File

// CountInFile returns how many squares are occupied in the chessboard,
// within the given file.
func CountInFile(cb Chessboard, file string) int {
	// Return a count of zero if the given file cannot be found in the map.
	if _, exists := cb[file]; !exists {
		return 0
	}

	occupied := 0
	for _, hasPiece := range cb[file] {
		if hasPiece {
			occupied += 1
		}
	}
	return occupied
}

// CountInRank returns how many squares are occupied in the chessboard,
// within the given rank.
func CountInRank(cb Chessboard, rank int) int {
	// Return a count of zero if the given rank is not between 1 and 8, inclusive.
	if rank < 1 || 8 < rank {
		return 0
	}

	occupied := 0
	for _, file := range cb {
		if file[rank-1] {
			occupied += 1
		}
	}
	return occupied
}

// CountAll should count how many squares are present in the chessboard.
func CountAll(cb Chessboard) int {
	squares := 0
	for _, file := range cb {
		for range file {
			squares += 1
		}
	}
	return squares
}

// CountOccupied returns how many squares are occupied in the chessboard.
func CountOccupied(cb Chessboard) int {
	occupied := 0
	for _, file := range cb {
		for _, hasPiece := range file {
			if hasPiece {
				occupied += 1
			}
		}
	}
	return occupied
}
