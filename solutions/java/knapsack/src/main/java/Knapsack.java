import java.util.List;

class Knapsack {

    int maximumValue(int maximumWeight, List<Item> items) {
        return max(items.size() - 1, maximumWeight, items).value;
    }

    private Item max(int i, int maxWeight, List<Item> items) {
        if (i < 0 || maxWeight == 0)
            return new Item(0, 0);

        if (items.get(i).weight > maxWeight) // item cannot fit in the bag
            return max(i - 1, maxWeight, items);

        Item prev = max(i - 1, maxWeight, items);
        Item next = max(i - 1, maxWeight - items.get(i).weight, items);

        next.value += items.get(i).value;
        if (next.value > prev.value) {
            next.weight += items.get(i).weight;
            return next;
        }
        return prev;
    }

}