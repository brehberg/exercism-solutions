def append(list1, list2):
    """given two lists, add all items in the second list to the end of the first list"""
    return foldl(lambda acc, item: acc + [item], list2, list1)


def concat(lists):
    """given a series of lists, combine all items in all lists into one flattened list"""
    return foldl(lambda acc, list: append(acc, list), lists, [])


def filter(predicate, list):
    """given a predicate and a list, return the list of all items for which predicate(item) is True"""
    return foldl(lambda acc, item: acc + [item] if predicate(item) else acc, list, [])


def length(list):
    """given a list, return the total number of items within it"""
    return foldl(lambda acc, _: acc + 1, list, 0)


def map(function, list):
    """given a function and a list, return the list of the results of applying function(item) on all items"""
    return foldl(lambda acc, item: acc + [function(item)], list, [])


def foldl(function, list, initial):
    """given a function, a list, and initial accumulator, fold (reduce) each item into the accumulator from the left"""
    if not list:
        return initial
    head, *tail = list
    return foldl(function, tail, function(initial, head))


def foldr(function, list, initial):
    """given a function, a list, and an initial accumulator, fold (reduce) each item into the accumulator from the right"""
    return foldl(function, reverse(list), initial)


def reverse(list):
    """given a list, return a list with all the original items, but in reversed order"""
    return foldl(lambda acc, item: [item] + acc, list, [])
