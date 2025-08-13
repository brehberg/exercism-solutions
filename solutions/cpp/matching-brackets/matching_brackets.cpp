#include "matching_brackets.h"
#include <map>
#include <vector>
#include <stack>
#include <algorithm>

namespace matching_brackets
{
    using namespace std;

    map<char, char> matches = {
        {'[', ']'},
        {'{', '}'},
        {'(', ')'}};

    bool check(string input)
    {
        stack<char> closerNeeded;

        // determine if matching bracket map contains the specified char value
        auto find_value = [](char c)
        {
            return find_if(matches.begin(), matches.end(), [c](const auto &mo)
                           { return mo.second == c; });
        };

        for (auto &c : input)
        {
            if (find_value(c) != matches.end())
            {
                // closing bracket was found, is it the next expected value on stack?
                if (closerNeeded.empty() || closerNeeded.top() != c)
                    return false;
                closerNeeded.pop();
            }
            else if (matches.find(c) != matches.end())
            {
                // opening bracket was found, add matching closing brack to the stack
                closerNeeded.push(matches[c]);
            }
        }

        return closerNeeded.empty();
    }

} // namespace matching_brackets
