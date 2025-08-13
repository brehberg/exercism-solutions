//@ts-check
//
// Cleans up user-entered phone numbers so that they can be sent SMS messages.
//

export const clean = (/** @type {string} */ phoneNumber) => {
  if (phoneNumber.match(/[a-z]/i)) throw new Error("Letters not permitted");
  if (phoneNumber.match(/[@:!]/)) throw new Error("Punctuations not permitted");

  // extract all digits from the given string
  const digits = phoneNumber.match(/\d/g)?.map(Number) || [];
  if (digits.length > 11) throw new Error("More than 11 digits");
  if (digits.length < 10) throw new Error("Incorrect number of digits");

  // all NANP-numbers share the same country code
  if (digits.length === 11)
    if (digits[0] === 1) digits.shift();
    else throw new Error("11 digits must start with 1");

  // area and exchange codes only digits from 2 through 9
  if (digits[0] === 0) throw new Error("Area code cannot start with zero");
  if (digits[0] === 1) throw new Error("Area code cannot start with one");
  if (digits[3] === 0) throw new Error("Exchange code cannot start with zero");
  if (digits[3] === 1) throw new Error("Exchange code cannot start with one");

  return digits.join("");
};
