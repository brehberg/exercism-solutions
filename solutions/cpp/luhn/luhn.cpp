#include "luhn.h"

namespace luhn
{
    bool valid(const std::string &input)
    {
        int sum{0}, count{0}, factor{2};
        for (size_t i = input.size(); i > 0; i--)
        {
            char letter = input[i - 1];
            if (letter == ' ')
            {
                continue;
            }
            if (letter < '0' || letter > '9')
            {
                return false;
            }

            factor = 3 - factor;
            int digit = int(letter - '0') * factor;
            sum += (digit > 9) ? digit - 9 : digit;
            count += 1;
        }
        return count > 1 && sum % 10 == 0;
    }
} // namespace luhn
