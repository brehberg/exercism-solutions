using System;

// row and column indexes representing a position in the matrix
using Position = (int row, int col);

public class SpiralMatrix
{
    // sequence of directions to fill the matrix in clockwise order
    private enum Direction
    {
        Right,
        Down,
        Left,
        Up,
    }

    // GetMatrix returns a square matrix of a given size that is filled with natural numbers,
    // starting from 1 in the top-left corner, increasing in an inward, clockwise spiral order.
    public static int[,] GetMatrix(int size)
    {
        var spiral = new int[size, size];
        var pos = new Position(0, 0);
        var dir = Direction.Right;

        for (int val = 1; val <= size * size; val++)
        {
            // update value at this position and check for valid next move
            spiral[pos.row, pos.col] = val;
            var next = adjustPosition(pos, dir);

            if (next.col >= size || // position beyond last column, turn down instead
                next.row >= size || // position beyond last row, turn left instead
                next.col < 0 ||     // position beyond first column, turn up instead
                next.row < 0 ||     // position beyond first row, turn right instead
                spiral[next.row, next.col] > 0) // position is filled, turn instead
            {
                dir = (Direction)(((int)dir + 1) % 4);
            }
            pos = adjustPosition(pos, dir);
        }
        return spiral;
    }

    // adjustPosition returns a new position based on the current direction
    private static Position adjustPosition(Position pos, Direction dir) => dir switch
    {
        Direction.Right => (pos.row, pos.col + 1), // move right to next column
        Direction.Down => (pos.row + 1, pos.col),  // move down to next row
        Direction.Left => (pos.row, pos.col - 1),  // move left to previous column
        Direction.Up => (pos.row - 1, pos.col),    // move up to previous row
        _ => throw new ArgumentException("invalid direction"),
    };
}
