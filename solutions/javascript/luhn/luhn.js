//@ts-check
/**
 * Given a number determine whether or not it is valid per the Luhn formula.
 *
 * @param {String} input
 * @returns {Boolean}
 */
export const valid = (input) => {
  const digits = input.replace(/\s/g, "");
  if (digits.length < 2 || digits.match(/\D/)) return false;

  let sum = 0;
  let odd = true;
  for (const d of digits.split("").reverse().map(Number)) {
    sum += odd ? d : [0, 2, 4, 6, 8, 1, 3, 5, 7, 9][d];
    odd = !odd;
  }
  return sum % 10 == 0;
};
