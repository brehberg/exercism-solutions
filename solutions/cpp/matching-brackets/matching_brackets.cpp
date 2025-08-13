#include "matching_brackets.h"
#include <algorithm>
#include <stack>
#include <unordered_map>

namespace matching_brackets
{
    using namespace std;

    const unordered_map<char, char> matches = {
        {'[', ']'},
        {'{', '}'},
        {'(', ')'},
    };

    // determine if matching bracket map contains the specified char value
    const auto find_value(char value)
    {
        return find_if(matches.begin(), matches.end(), [value](const auto &mo)
                       { return mo.second == value; });
    };

    bool check(string const &input)
    {
        stack<char> closer_needed{};

        for (auto &c : input)
        {
            if (matches.find(c) != matches.end())
            {
                // opening bracket was found, add matching closing value to the stack
                closer_needed.push(matches.at(c));
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
