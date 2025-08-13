package binarysearch

// SearchInts returns the index for a key in the slice using the binary
// search algorithm. It returns -1 if the key is not in the slice.
func SearchInts(list []int, key int) int {
	return subSearchInts(list, key, 0, len(list)-1)
}

func subSearchInts(list []int, key, low, high int) int {
	if low > high {
		return -1
	}

	mid := (low + high) / 2
	if list[mid] > key {
		return subSearchInts(list, key, low, mid-1)
	} else if list[mid] < key {
		return subSearchInts(list, key, mid+1, high)
	}
	return mid
}
