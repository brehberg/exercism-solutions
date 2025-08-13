#include "list_ops.h"

using namespace std;
namespace list_ops
{
    // given two lists, add all items in the second list to the end of the first list
    void append(vector<int> &list1, vector<int> list2)
    {
        for (auto &item : list2)
        {
            list1.emplace_back(item);
        }
    }
    void append(vector<vector<int>> &list1, vector<vector<int>> list2)
    {
        for (auto &item : list2)
        {
            list1.emplace_back(item);
        }
    }

    // given a series of lists, combine all items in all lists into one flattened list
    vector<int> concat(vector<vector<int>> lists)
    {
        vector<int> result;
        for (auto &list : lists)
        {
            append(result, list);
        }
        return result;
    }
    vector<vector<int>> concat(vector<vector<vector<int>>> lists)
    {
        vector<vector<int>> result;
        for (auto &list : lists)
        {
            append(result, list);
        }
        return result;
    }

    // given a list and a predicate, return the list of all items for which predicate(item) is True
    vector<int> filter(vector<int> list, bool (*pred)(int))
    {
        vector<int> result;
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
    size_t length(vector<int> list)
    {
        size_t count = 0;
        count = list.size();
        // for (auto &item : list)
        // {
        //     count += 1;
        // }
        return count;
    }

    // given a list and a function, return the list of the results of applying function(item) on all items
    vector<int> map(vector<int> list, int (*func)(int))
    {
        vector<int> result;
        for (auto &item : list)
        {
            result.emplace_back(func(item));
        }
        return result;
    }

    // given a list, initial accumulator, and a function, fold (reduce) each item into the accumulator from the left
    int foldl(vector<int> list, int initial, int (*func)(int, int))
    {
        auto result = initial;
        for (auto &item : list)
        {
            result = func(result, item);
        }
        return result;
    }
    double foldl(vector<double> list, double initial, double (*func)(double, double))
    {
        auto result = initial;
        for (auto &item : list)
        {
            result = func(result, item);
        }
        return result;
    }

    // given a list, initial accumulator, and a function, fold (reduce) each item into the accumulator from the right
    int foldr(vector<int> list, int initial, int (*func)(int, int))
    {
        auto result = initial;
        for (auto &item : reverse(list))
        {
            result = func(result, item);
        }
        return result;
    }
    double foldr(vector<double> list, double initial, double (*func)(double, double))
    {
        auto result = initial;
        for (auto &item : reverse(list))
        {
            result = func(result, item);
        }
        return result;
    }

    // given a list, return a list with all the original items, but in reversed order
    vector<int> reverse(vector<int> list)
    {
        vector<int> result;
        for (auto &item : list)
        {
            result.insert(result.begin(), item);
        }
        return result;
    }
    vector<double> reverse(vector<double> list)
    {
        vector<double> result;
        for (auto &item : list)
        {
            result.insert(result.begin(), item);
        }
        return result;
    }
    vector<vector<int>> reverse(vector<vector<int>> lists)
    {
        vector<vector<int>> result;
        for (auto &list : lists)
        {
            result.insert(result.begin(), list);
        }
        return result;
    }

} // namespace list_ops
