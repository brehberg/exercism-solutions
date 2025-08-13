# Input should be the array of items having a weight and value.
# As well as the maximum weight of knapsack's carrying capacity.

# Because of the way addition is defined on null and because of the way
# setpath works, there is no need to initialize the matrix m in detail.
.items as $items
| .maximumWeight as $maxWeight
| reduce range(0; $items|length) as $i                 # i is the number of items 
    (null;                                 # state: m[i][j] is an array of values
    $items[$i] as $item
    | reduce range(0; $maxWeight+1) as $j
        (.;
        if $item.weight > $j then               # item cannot fit in the knapsack
            .[$i+1][$j] = .[$i][$j]
        else
            .[$i][$j] as $old                  # option 1: do not add this object
            | (.[$i][$j-$item.weight]+$item.value) as $new     # option 2: add it
            | .[$i+1][$j] = (if $old > $new then $old    # do not add this object
                             else $new end)              # add it
        end))
| .|last[$maxWeight] // 0
