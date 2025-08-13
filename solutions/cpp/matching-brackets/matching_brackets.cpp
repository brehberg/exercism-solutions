#include "matching_brackets.h"
#include <map>
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
        stack<char> closer_needed;

        auto find_value = [](char c)
        {
            // determine if matching bracket map contains the specified char value
            return find_if(matches.begin(), matches.end(), [c](const auto &mo)
                           { return mo.second == c; });
        };

        for (auto &c : input)
        {
            if (matches.find(c) != matches.end())
            {
                // opening bracket was found, add matching closing value to the stack
                closer_needed.push(matches[c]);
            }
            else if (find_value(c) != matches.end())
            {
                // closing bracket was found, is it the next expected value on stack?
                if (closer_needed.empty() || closer_needed.top() != c)
                    return false;
                closer_needed.pop();
            }
        }

        return closer_needed.empty();
    }

} // namespace matching_brackets
