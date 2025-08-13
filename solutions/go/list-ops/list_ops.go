package listops

// IntList is an abstraction of a list of integers which we can define methods on
type IntList []int

func (s IntList) Foldl(fn func(int, int) int, initial int) int {
	result := initial
	for _, item := range s {
		result = fn(result, item)
	}
	return result
}

func (s IntList) Foldr(fn func(int, int) int, initial int) int {
	result := initial
	for i := range s {
		result = fn(s[len(s)-1-i], result)
	}
	return result
}

func (s IntList) Filter(fn func(int) bool) IntList {
	filtered := IntList{}
	for _, item := range s {
		if fn(item) {
			filtered = append(filtered, item)
		}
	}
	return filtered
}

func (s IntList) Length() int {
	return s.Foldl(func(count, _ int) int { return count + 1 }, 0)
}

func (s IntList) Map(fn func(int) int) IntList {
	mapped := IntList{}
	for _, item := range s {
		mapped = append(mapped, fn(item))
	}
	return mapped
}

func (s IntList) Reverse() IntList {
	reversed := IntList{}
	for i := range s {
		reversed = append(reversed, s[len(s)-1-i])
	}
	return reversed
}

func (s IntList) Append(lst IntList) IntList {
	return append(s, lst...)
}

func (s IntList) Concat(lists []IntList) IntList {
	concatenated := s
	for _, list := range lists {
		concatenated = concatenated.Append(list)
	}
	return concatenated
}
