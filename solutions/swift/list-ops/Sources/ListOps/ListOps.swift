// append: given two lists, add all items in the second list to the end of the first list
func append<T>(_ first: [T], _ second: [T]) -> [T] {
  return foldLeft(second, accumulated: first, combine: { $0 + [$1] })
}

// concat: given a series of lists, combine all items in all lists into one flattened list
func concat<T>(_ lists: [T]...) -> [T] {
  return foldLeft(lists, accumulated: [], combine: append)
}

// filter: given a list and a predicate, return the list of all items for which predicate(item) is True
func filter<T>(_ list: [T], predicate: (T) -> Bool) -> [T] {
  return foldLeft(list, accumulated: [], combine: { predicate($1) ? $0 + [$1] : $0 })
}

// length: given a list, return the total number of items within it
func length<T>(_ list: [T]) -> Int {
  return foldLeft(list, accumulated: 0, combine: { (count, _) in count + 1 })
}

// map: given a list and a function, return the list of the results of applying function(item) on all items
func map<T>(_ list: [T], function: (T) -> T) -> [T] {
  return foldLeft(list, accumulated: [], combine: { $0 + [function($1)] })
}

// foldLeft: given a list, initial accumulator, and a function, fold (reduce) each item
//           into the accumulator from the left using function(accumulator, item)
func foldLeft<TIn, TOut>(
  _ list: [TIn], accumulated: TOut, combine: (TOut, TIn) -> TOut
) -> TOut {
  if list.isEmpty { return accumulated }
  return foldLeft(
    Array(list.dropFirst()), accumulated: combine(accumulated, list[0]), combine: combine)
}

// foldRight: given a list, initial accumulator, and a function, fold (reduce) each item
//            into the accumulator from the right using function(item, accumulator)
func foldRight<TIn, TOut>(
  _ list: [TIn], accumulated: TOut, combine: (TIn, TOut) -> TOut
) -> TOut {
  return foldLeft(reverse(list), accumulated: accumulated, combine: { combine($1, $0) })
}

// reverse: given a list, return a list with all the original items, but in reversed order
func reverse<T>(_ list: [T]) -> [T] {
  return foldLeft(list, accumulated: [], combine: { [$1] + $0 })
}
