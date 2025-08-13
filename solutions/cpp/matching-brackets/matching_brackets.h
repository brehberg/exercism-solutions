#if !defined(MATCHING_BRACKETS_H)
#define MATCHING_BRACKETS_H

#include <string>

namespace matching_brackets
{
    // Checks that all the brackets and braces in the string
    // are matched correctly, and nested correctly
    bool check(std::string const &);
} // namespace matching_brackets

#endif // MATCHING_BRACKETS_H