#include "parallel_letter_frequency.h"

#include <cctype>
#include <execution>

namespace parallel_letter_frequency
{
    using namespace std;

    map_t count_letter_frequencies(const string_view &text)
    {
        map_t frequency{};
        for (char c : text)
        {
            if (isalpha(c))
            {
                frequency[tolower(c)] += 1;
            }
        }
        return frequency;
    }

    map_t frequency(const texts_t &texts)
    {
        map_t result{};
        for_each(execution::par_unseq, texts.begin(), texts.end(),
                 [&](const auto &text)
                 {
                     auto frequency = count_letter_frequencies(text);
                     for (const auto [letter, count] : frequency)
                     {
                         result[letter] += count;
                     }
                 });

        return result;
    }
}
