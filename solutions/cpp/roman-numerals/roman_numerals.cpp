#include "roman_numerals.h"

namespace roman_numerals
{
    std::string ones[] = {"", "I", "II", "III", "IV",
                          "V", "VI", "VII", "VIII", "IX"};
    std::string tens[] = {"", "X", "XX", "XXX", "XL",
                          "L", "LX", "LXX", "LXXX", "XC"};
    std::string hund[] = {"", "C", "CC", "CCC", "CD",
                          "D", "DC", "DCC", "DCCC", "CM"};
    std::string thou[] = {"", "M", "MM", "MMM"};

    std::string convert(int num)
    {
        return thou[num / 1000] +
               hund[num % 1000 / 100] +
               tens[num % 100 / 10] +
               ones[num % 10];
    }
} // namespace roman_numerals
