#include "dnd_character.h"
#include <random>

namespace dnd_character
{
    namespace
    {
        // pseudorandom number generator using Mersenne Twister
        auto &prng()
        {
            thread_local std::mt19937 prng{std::random_device{}()};
            return prng;
        }
    }

    int ability()
    {
        // roll 4 dice and discard the smallest
        std::uniform_int_distribution<int> dist(1, 6);
        int sum = 0;
        int min = 7;
        for (int i = 0; i < 4; ++i)
        {
            int die = dist(prng());
            sum += die;
            min = std::min(min, die);
        }
        return sum - min;
    }

    int modifier(int ability)
    {
        return std::floor(ability / 2 - 5);
    }
} // namespace dnd_character
