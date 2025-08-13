// append: given two lists, add all items in the second list to the end of the first list
func append<T>(_ first: [T], _ second: [T]) -> [T] {
  return foldLeft(second, accumulated: first, combine: { (acc, item) in acc + [item] })
}

// concat: given a series of lists, combine all items in all lists into one flattened list
func concat<T>(_ lists: [T]...) -> [T] {
  return foldLeft(lists, accumulated: [], combine: append)
}

// filter: given a list and a predicate, return the list of all items for which predicate(item) is True
func filter<T>(_ list: [T], predicate pred: (T) -> Bool) -> [T] {
  return foldLeft(
    list, accumulated: [], combine: { (acc, item) in pred(item) ? acc + [item] : acc })
}

// length: given a list, return the total number of items within it
func length<T>(_ list: [T]) -> Int {
  return foldLeft(list, accumulated: 0, combine: { (count, _) in count + 1 })
}

// map: given a list and a function, return the list of the results of applying function(item) on all items
func map<T>(_ list: [T], function fn: (T) -> T) -> [T] {
  return foldLeft(list, accumulated: [], combine: { (acc, item) in acc + [fn(item)] })
}

// foldLeft: given a list, initial accumulator, and a function, fold (reduce) each item
//           into the accumulator from the left using function(accumulator, item)
func foldLeft<TIn, TOut>(
  _ list: [TIn], accumulated initial: TOut, combine fn: (TOut, TIn) -> TOut
) -> TOut {
  if list.isEmpty { return initial }
  return foldLeft(Array(list.dropFirst()), accumulated: fn(initial, list[0]), combine: fn)
}

// foldRight: given a list, initial accumulator, and a function, fold (reduce) each item
//            into the accumulator from the right using function(item, accumulator)
func foldRight<TIn, TOut>(
  _ list: [TIn], accumulated initial: TOut, combine fn: (TIn, TOut) -> TOut
) -> TOut {
  return foldLeft(
    reverse(list), accumulated: initial, combine: { (acc, item) in fn(item, acc) })
}

// reverse: given a list, return a list with all the original items, but in reversed order
func reverse<T>(_ list: [T]) -> [T] {
  return foldLeft(list, accumulated: [], combine: { (acc, item) in [item] + acc })
}
