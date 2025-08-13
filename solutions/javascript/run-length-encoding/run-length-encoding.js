//@ts-check

/**
 * Encode returns a string where consecutive elements are represented as a count and value.
 *
 * @param {string} input plaintext
 * @return {string} ciphertext
 */
export const encode = (input) => {
  let result = "";
  if (!input) return result;

  const encode_pairs = (/** @type {number} */ n, /** @type {string} */ c) => {
    return n === 1 ? c : n + c;
  };

  let prev = input[0];
  let count = 1;

  for (let i = 1; i < input.length; i++) {
    const char = input[i];
    if (char === prev) {
      count += 1;
      continue;
    }
    result += encode_pairs(count, prev);
    prev = char;
    count = 1;
  }

  return (result += encode_pairs(count, prev));
};

/**
 * Decode returns a string that has been reconstructed from the input into its original form.
 *
 * @param {string} input ciphertext
 * @return {string} plaintext
 */
export const decode = (input) => {
  let result = "";
  if (!input) return result;

  const groups = input.match(/\d*./g) || [];
  for (const group of groups) {
    const offset = group.length - 1;
    const count = !offset ? 1 : Number(group.slice(0, offset));
    result += group.slice(offset).repeat(count);
  }

  return result;
};
