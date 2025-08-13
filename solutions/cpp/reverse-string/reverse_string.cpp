#include "reverse_string.h"

using namespace std;

namespace reverse_string
{
    // Reverse an input string efficiently by swapping individual characters
    string reverse_string(string str)
    {
        size_t length = str.size();
        for (size_t i = 0; i < length / 2; i++)
        {
            swap(str[i], str[length - i - 1]);
        }
        return str;
    }
} // namespace reverse_string
