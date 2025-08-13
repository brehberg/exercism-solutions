//@ts-check

/**
 * Encode returns a string where consecutive elements are represented as a count and value.
 *
 * @param {string} input plaintext
 * @return {string} ciphertext
 */
export const encode = (input) =>
  input.replace(/(.)\1+/g, (match, char) => `${match.length}${char}`);

/**
 * Decode returns a string that has been reconstructed from the input into its original form.
 *
 * @param {string} input ciphertext
 * @return {string} plaintext
 */
export const decode = (input) =>
  input.replace(/(\d+)(.)/g, (_, count, char) => char.repeat(count));
