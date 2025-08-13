//
// Add the mine counts to empty squares in a completed Minesweeper board
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
  const rows = input.length;
  const cols = input[0].length;

  // create matrix of minefield as 2D array
  let minefield = input.map((str) => [...str]);

  const increment_neighbor = (row, col) => {
    if (row < 0 || row >= rows) return;
    if (col < 0 || col >= cols) return;

    const value = minefield[row][col];
    if (value === MINE) return;

    minefield[row][col] = Number(value) + 1;
  };

  const increment_neighbors = (row, col) => {
    NEIGHBORS.forEach(([r, c]) => {
      increment_neighbor(row + r, col + c);
    });
  };

  // Traverse minefield to find mine and increment cells around it
  minefield.forEach((row, r) => {
    row.forEach((cell, c) => {
      if (cell === MINE) increment_neighbors(r, c);
    });
  });

  return minefield.map((row) => row.join(""));
};
