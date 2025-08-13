#pragma once
#include <vector>

using namespace std;

namespace list_ops
{
    // given two lists, add all items in the second list to the end of the first list
    template <typename T>
    void append(vector<T> &list1, vector<T> list2)
    {
        for (auto &item : list2)
        {
            list1.emplace_back(item);
        }
    }

    // given a series of lists, combine all items in all lists into one flattened list
    template <typename T>
    vector<T> concat(vector<vector<T>> lists)
    {
        vector<T> result{};
        for (auto &list : lists)
        {
            append(result, list);
        }
        return result;
    }

    // given a list and a predicate, return the list of all items for which predicate(item) is True
    template <typename T, typename P>
    vector<T> filter(vector<T> list, P &&pred)
    {
        vector<T> result{};
        for (auto &item : list)
        {
            if (pred(item))
            {
                result.emplace_back(item);
            }
        }
        return result;
    }

    // given a list, return the total number of items within it
    template <typename T>
    size_t length(vector<T> list)
    {
        size_t count = 0;
        for (auto &item __attribute__((unused)) : list)
        {
            count += 1;
        }
        return count;
    }

    // given a list and a function, return the list of the results of applying function(item) on all items
    template <typename T, typename F>
    vector<T> map(vector<T> list, F &&func)
    {
        vector<T> result{};
        for (auto &item : list)
        {
            result.emplace_back(func(item));
        }
        return result;
    }

    // given a list, initial accumulator, and a function, fold (reduce) each item into the accumulator from the left
    template <typename T, typename F>
    int foldl(vector<T> list, T initial, F &&func)
    {
        auto result = initial;
        for (auto &item : list)
        {
            result = func(result, item);
        }
        return result;
    }

    // given a list, return a list with all the original items, but in reversed order
    template <typename T>
    vector<T> reverse(vector<T> list)
    {
        vector<T> result{};
        for (auto &item : list)
        {
            result.insert(result.begin(), item);
        }
        return result;
    }

    // given a list, initial accumulator, and a function, fold (reduce) each item into the accumulator from the right
    template <typename T, typename F>
    int foldr(vector<T> list, T initial, F &&func)
    {
        auto result = initial;
        for (auto &item : reverse(list))
        {
            result = func(result, item);
        }
        return result;
    }
} // namespace list_ops
