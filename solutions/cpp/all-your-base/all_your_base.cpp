#include "all_your_base.h"
#include <cmath>
#include <algorithm>

namespace all_your_base
{
    using namespace std;

    vector<unsigned int> convert(int from_base, vector<unsigned int> from_digits, int to_base)
    {
        if (from_base < 2)
            throw invalid_argument("invalid input base");
        if (to_base < 2)
            throw invalid_argument("invalid output base");

        // convert sequence of digits in input base to whole integer value
        int value{0};
        auto positional_offset = from_digits.size();

        for (auto digit : from_digits)
        {
            if ((int)digit >= from_base)
                throw invalid_argument("digit out of range");
            value += digit * pow(from_base, --positional_offset);
        }

        // convert whole integer value to sequence of digits in output base
        vector<unsigned int> to_digits{};
        if (value == 0)
            return to_digits;

        while (value >= to_base)
        {
            to_digits.emplace_back(value % to_base);
            value /= to_base;
        }
        to_digits.emplace_back(value);
        reverse(to_digits.begin(), to_digits.end());

        return to_digits;
    };
} // namespace all_your_base
