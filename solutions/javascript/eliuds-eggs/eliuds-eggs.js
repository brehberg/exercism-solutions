//@ts-check
export const eggCount = (/** @type {number} */ displayValue) => {
  let count = 0;
  for (; displayValue; count++) displayValue &= displayValue - 1;
  return count;
};
