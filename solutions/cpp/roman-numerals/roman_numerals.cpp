#include "roman_numerals.h"

namespace roman_numerals
{
    std::string thou[10] = {"", "M", "MM", "MMM"};
    std::string hund[10] = {"", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"};
    std::string tens[10] = {"", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "IC"};
    std::string ones[10] = {"", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"};

    std::string convert(int num)
    {
        return thou[num / 1000] +
               hund[num % 1000 / 100] +
               hund[num % 100 / 10] +
               ones[num % 10];
    }
} // namespace roman_numerals
