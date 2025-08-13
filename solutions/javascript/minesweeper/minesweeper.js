//
// Add the mine counts to empty squares in a completed Minesweeper board.
//
const NEIGHBORS = [
  [-1, -1],
  [-1, 0],
  [-1, 1],
  [0, -1],
  [0, 1],
  [1, -1],
  [1, 0],
  [1, 1],
];
const MINE = "*";

export const annotate = (input) => {
  // check for no rows or columns
  if (!input.length || input[0] === "") return input;

  // create matrix of minefield as 2D array
  let minefield = input.map((str) => [...str]);
  const rows = minefield.length;
  const cols = minefield[0].length;

  const incrementNeighbor = (row, col) => {
    // check neighbor is in the minefield
    if (row < 0 || row >= rows) return;
    if (col < 0 || col >= cols) return;

    // check neighbor is not another mine
    const value = minefield[row][col];
    if (value === MINE) return;

    // increment the count for this cell
    minefield[row][col] = Number(value) + 1;
  };

  const incrementNeighbors = (row, col) => {
    NEIGHBORS.forEach(([rowOffset, colOffset]) => {
      incrementNeighbor(row + rowOffset, col + colOffset);
    });
  };

  // Traverse minefield to find mine and increment cells around it
  minefield.forEach((row, r) => {
    row.forEach((cell, c) => {
      if (cell === MINE) incrementNeighbors(r, c);
    });
  });

  return minefield.map((row) => row.join(""));
};
