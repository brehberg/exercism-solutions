// @ts-check
export class List {
  constructor(/** @type {any[] | undefined} list */ list) {
    this.values = list || [];
  }

  /**
   * Given a list, add all items in the provided list to the end of this list
   *
   * @param {List} list
   * @return {List} appended values
   */
  append(list) {
    let appended = this.values;
    for (const item of list.values) {
      appended = [...appended, item];
    }
    return new List(appended);
  }

  /**
   * Given a list of lists, combine all items in all lists into one flattened list
   *
   * @param {List} lists
   * @return {List} concatenated values
   */
  concat(lists) {
    let concatenatedList = new List(this.values);
    for (const list of lists.values) {
      concatenatedList = concatenatedList.append(list);
    }
    return concatenatedList;
  }

  /**
   * Given a predicate, return the list of all items for which predicate(item) is True
   *
   * @param {(item: any) => boolean} pred
   * @return {List} filtered values
   */
  filter(pred) {
    let filtered = [];
    for (const item of this.values) {
      filtered = pred(item) ? [...filtered, item] : filtered;
    }
    return new List(filtered);
  }

  /**
   * Given a function, return the list of the results of applying function(item) on all items
   *
   * @param {(item: any) => any} func
   * @return {List} mapped values
   */
  map(func) {
    let mapped = [];
    for (const item of this.values) {
      mapped = [...mapped, func(item)];
    }
    return new List(mapped);
  }

  /**
   * Return the total number of items within it
   *
   * @return {number} count of values
   */
  length() {
    let count = 0;
    for (const _ of this.values) {
      count += 1;
    }
    return count;
  }

  /**
   * Given a function and initial accumulator, fold each item into the accumulator from the left
   *
   * @param {(acc: any, item: any) => any} func
   * @param {any} initial
   * @return {any} accumulated value
   */
  foldl(func, initial) {
    let final = initial;
    for (const item of this.values) {
      final = func(final, item);
    }
    return final;
  }

  /**
   * Given a function and initial accumulator, fold each item into the accumulator from the right
   *
   * @param {(acc: any, item: any) => any} func
   * @param {any} initial
   * @return {any} accumulated value
   */
  foldr(func, initial) {
    let final = initial;
    for (const item of this.reverse().values) {
      final = func(final, item);
    }
    return final;
  }

  /**
   * Return a list with all the original items, but in reversed order
   *
   * @return {List} reversed values
   */
  reverse() {
    let reversed = [];
    for (const item of this.values) {
      reversed = [item, ...reversed];
    }
    return new List(reversed);
  }
}
