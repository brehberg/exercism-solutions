module ListOps

let rec foldl folder state list =
    // given a function, an initial accumulator, and a list
    // reduce each item into the accumulator from the left
    match list with
    | [] -> state
    | x :: xs -> foldl folder (folder state x) xs

let reverse list =
    // given a list, return a list with all the original items, but in reversed order
    foldl (fun acc item -> item :: acc) [] list

let rec foldr folder state list =
    // given a function, an initial accumulator, and a list
    // reduce each item into the accumulator from the right
    list |> reverse |> foldl (fun acc item -> folder item acc) state

let length list =
    // given a list, return the total number of items within it
    foldl (fun acc _ -> acc + 1) 0 list

let map f list =
    // given a function and a list, return the list of the results of applying function on all items
    foldr (fun item acc -> f item :: acc) [] list

let filter f list =
    // given a predicate and a list, return the list of all items for which predicate is True
    foldr (fun item acc -> if f item then item :: acc else acc) [] list

let append xs ys =
    // given two lists, add all items in the second list to the end of the first list
    foldr (fun item acc -> item :: acc) ys xs

let concat xs =
    // given a series of lists, combine all items in all lists into one flattened list
    foldr append [] xs
