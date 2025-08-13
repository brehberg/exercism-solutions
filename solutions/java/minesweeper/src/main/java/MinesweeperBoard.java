import java.util.ArrayList;
import java.util.List;

final class MinesweeperBoard {

    private final List<String> minefield;
    private final int rows;
    private final int cols;
    private static final char MINE = '*';
    private static final char EMPTY = ' ';

    MinesweeperBoard(final List<String> input) {
        minefield = input;
        rows = minefield.size();
        cols = minefield.isEmpty() ? 0 : minefield.get(0).length();
    }

    List<String> withNumbers() {
        final List<String> result = new ArrayList<>(rows);
        for (int row = 0; row < rows; row++) {
            result.add(buildRowString(row));
        }
        return result;
    }

    private String buildRowString(final int row) {
        StringBuilder result = new StringBuilder(cols);
        for (int col = 0; col < cols; col++) {
            result.append(determineCellValue(row, col));
        }
        return result.toString();
    }

    private char determineCellValue(final int rowNum, final int colNum) {
        if (minefield.get(rowNum).charAt(colNum) == MINE) {
            return MINE;
        }
        final int mines = computeMineCountAround(rowNum, colNum);
        return mines == 0 ? EMPTY : Character.forDigit(mines, 10);
    }

    private int computeMineCountAround(final int row, final int col) {
        final int startRow = Math.max(row - 1, 0);
        final int endRow = Math.min(row + 1, rows - 1);
        final int startCol = Math.max(col - 1, 0);
        final int endCol = Math.min(col + 1, cols - 1);

        int result = 0;
        for (int r = startRow; r <= endRow; r++) {
            for (int c = startCol; c <= endCol; c++) {
                result += minefield.get(r).charAt(c) == MINE ? 1 : 0;
            }
        }
        return result;
    }

}