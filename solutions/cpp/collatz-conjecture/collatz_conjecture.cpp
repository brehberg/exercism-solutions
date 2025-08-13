#include "collatz_conjecture.h"
#include <stdexcept>

namespace collatz_conjecture
{
    unsigned int steps(int num)
    {
        if (num < 1)
            throw std::domain_error("Only positive numbers are allowed");

        unsigned int steps{0};
        for (; num > 1; steps++)
            num = (num % 2 == 0) ? num / 2 : 3 * num + 1;
        return steps;
    }

} // namespace collatz_conjecture
