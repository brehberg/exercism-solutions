//
// Compute Pascal's triangle up to a given number of rows.
//
export const rows = (n) => {
  const triangle = [];
  let row = [1];

  while (n > 0) {
    triangle.push(row);
    row = generateNextRow(row);
    n--;
  }
  return triangle;
};

const generateNextRow = (previous) => {
  const nextRow = [1];
  for (let i = 1; i < previous.length; i++) {
    nextRow.push(previous[i - 1] + previous[i]);
  }
  nextRow.push(1);
  return nextRow;
};
