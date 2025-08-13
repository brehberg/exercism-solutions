#include "raindrops.h"

namespace raindrops
{
    std::string convert(int number)
    {
        auto divisible_by = [number](int n)
        { return number % n == 0; };

        // string vector of all possible combinations of raindrop sounds
        std::vector<std::string> sounds =
            {"", "Pling", "Plang", "PlingPlang", "Plong",
             "PlingPlong", "PlangPlong", "PlingPlangPlong"};

        // determine index to access vector of raindrop sounds based on factors
        int index = divisible_by(3) + 2 * divisible_by(5) + 4 * divisible_by(7);
        return (index == 0) ? std::to_string(number) : sounds[index];
    };
} // namespace raindrops
