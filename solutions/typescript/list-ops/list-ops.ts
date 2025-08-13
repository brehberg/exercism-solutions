export class List {
  private constructor(readonly values: unknown[]) { }

  public static create(...values: unknown[]): List {
    // Does *not* construct any array literals ([]) in this solution.
    // Does *not* construct any arrays through new Array in this solution.
    // Does *not* use any of the Array.prototype methods in this solution.

    // Does use the destructuring and spreading (...) syntax from Iterable.
    return new List(values)
  }

  // Given a list, add all items in the provided list to the end of this list
  public append(list: List): List {
    return List.create(...this.values, ...list.values)
  }

  // Given a list of lists, combine all items in all lists into one flattened list
  public concat(lists: List): List {
    return lists.foldl<List, List>((final, list) =>
      final.append(list), List.create(...this.values));
  }

  // Given a predicate, return the list of all items for which predicate(item) is True
  public filter<T>(pred: (item: T) => boolean): List {
    return this.foldl<T, List>((final, item) =>
      pred(item) ? final.append(List.create(item)) : final, List.create())
  }

  // Given a function, return the list of the results of applying function(item) on all items
  public map<T>(func: (item: T) => T): List {
    return this.foldl<T, List>((final, item) =>
      final.append(List.create(func(item))), List.create())
  }

  // Return the total number of items within it
  public length(): number {
    return this.foldl<unknown, number>((count, _) => count += 1, 0);
  }

  // Given a function and initial accumulator, fold each item into the accumulator from the left
  public foldl<T, R>(func: (acc: R, item: T) => R, initial: R): R {
    let accumulator = initial
    this.forEach((item) => { accumulator = func(accumulator, item as T) })
    return accumulator
  }

  // Given a function and initial accumulator, fold each item into the accumulator from the right
  public foldr<T, R>(func: (acc: R, item: T) => R, initial: R): R {
    return this.reverse().foldl<T, R>(func, initial)
  }

  // Return a list with all the original items, but in reversed order
  public reverse(): List {
    return this.foldl<unknown, List>((final, item) =>
      List.create(item).append(final), List.create())
  }

  forEach(func: (item: unknown) => void): void {
    for (const item of this.values) { func(item) }
  }
}
