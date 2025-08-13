//
// Calculate the Hamming Distance between two DNA strands.
//
export const compute = (a, b) => {
  if (a.length != b.length) {
    throw new Error("strands must be of equal length");
  }

  let diffCount = 0;
  for (let i = 0; i < a.length; i++) {
    diffCount += a[i] != b[i];
  }
  return diffCount;
};
