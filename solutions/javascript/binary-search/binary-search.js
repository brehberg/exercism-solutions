//
// Implements a binary search algorithm.
//
export const find = (input, value) => {
  let low = 0,
    high = input.length - 1;

  while (low <= high) {
    let mid = Math.floor(low + (high - low) / 2);

    if (input[mid] < value) low = mid + 1;
    else if (input[mid] > value) high = mid - 1;
    else return mid;
  }

  throw new Error("Value not in array");
};
