package knapsack

type Item struct {
	Weight, Value int
}

// Knapsack takes in a maximum carrying capacity and a collection of items
// and returns the maximum value that can be carried by the knapsack
// given that the knapsack can only carry a maximum weight given by maximumWeight
func Knapsack(maximumWeight int, items []Item) int {
	_, v := m(len(items)-1, maximumWeight, items)
	return v
}

func m(i, maxWeight int, wants []Item) (int, int) {
	if i < 0 || maxWeight == 0 {
		return 0, 0
	} else if wants[i].Weight > maxWeight {
		return m(i-1, maxWeight, wants)
	}

	oldWeight, oldValue := m(i-1, maxWeight, wants)
	newWeight, newValue := m(i-1, maxWeight-wants[i].Weight, wants)
	newValue += wants[i].Value

	if newValue > oldValue {
		return newWeight + wants[i].Weight, newValue
	}
	return oldWeight, oldValue
}
