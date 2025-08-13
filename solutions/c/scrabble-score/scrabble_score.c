#include "scrabble_score.h"

#include <ctype.h>

unsigned int score(const char *input)
{
    static int LETTER_VALUE[] =
        {1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3,
         1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10};

    int sum = 0;
    for (char c = *input; c != '\0'; c = *++input)
    {
        sum += LETTER_VALUE[toupper(c) - 65];
    }
    return sum;
}
