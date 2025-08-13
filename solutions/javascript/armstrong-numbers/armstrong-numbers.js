//@ts-check

/**
 *  isArmstrongNumber returns true if num is an Armstrong Number.
 *  An Armstrong number is a number that is the sum of its own
 *  digits each raised to the power of the number of digits.
 *
 * @param {number} num
 * @returns {boolean}
 */
export const isArmstrongNumber = (num) => {
  const digits = num.toString();
  let sum = 0;
  for (const d of digits) {
    sum += Number(d) ** digits.length;
  }
  return num == sum;
};
