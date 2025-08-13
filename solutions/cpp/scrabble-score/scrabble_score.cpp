#include "scrabble_score.h"

namespace scrabble_score
{
    using namespace std;

    static int A = toupper('A');
    static int LETTER_VALUE[] =
        {1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3,
         1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10};

    int score(string input)
    {
        int sum{0};
        for (char c : input)
        {
            if (isalpha(c))
            {
                sum += LETTER_VALUE[toupper(c) - A];
            }
        }
        return sum;
    }
} // namespace scrabble_score
