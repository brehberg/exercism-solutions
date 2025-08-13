/**
 * Given a number determine whether or not it is valid per the Luhn formula.
 *
 * @param {String} input
 * @returns {Boolean}
 */
export function valid(input: String): Boolean {
  const digits = input.replace(/\s/g, "");
  if (digits.length < 2 || digits.match(/\D/)) return false;

  let sum = 0;
  let odd = true;
  for (const d of digits.split("").reverse().map(Number)) {
    sum += odd ? d : d < 5 ? d * 2 : d * 2 - 9;
    odd = !odd;
  }
  return sum % 10 == 0;
}