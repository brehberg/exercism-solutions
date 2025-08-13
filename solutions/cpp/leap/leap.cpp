#include "leap.h"

namespace leap
{
    bool is_leap_year(int year)
    {
        auto is_divisible_by = [year](int n)
        {
            return year % n == 0;
        };

        return is_divisible_by(4) and
               (not is_divisible_by(100) or
                is_divisible_by(400));
    }
} // namespace leap
