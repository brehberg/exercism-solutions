#include "minesweeper.h"

namespace minesweeper
{
    vector<string> annotate(vector<string> input)
    {
        int rows = input.size();
        if (rows == 0)
            return input;

        int cols = input[0].length();
        if (cols == 0)
            return input;

        auto check_row = [input, rows, cols](int row, int col)
        {
            int count{0};
            if (row < 0)
                return count;
            if (row >= rows)
                return count;

            count += (input[row][col] == '*');
            count += (col - 1 >= 0 && input[row][col - 1] == '*');
            count += (col + 1 < cols && input[row][col + 1] == '*');
            return count;
        };

        auto output(input);
        for (int r = 0; r < rows; r++)
        {
            for (int c = 0; c < cols; c++)
            {
                if (input[r][c] == '*')
                    continue;

                int mines{0};
                mines += check_row(r, c);
                mines += check_row(r - 1, c);
                mines += check_row(r + 1, c);

                if (mines > 0)
                    output[r][c] = (char)((int)'0' + mines);
            }
        }
        return output;
    }
} // namespace minesweeper
