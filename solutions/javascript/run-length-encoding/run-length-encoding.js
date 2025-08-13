//@ts-check

/**
 * Encode returns a string where consecutive elements are represented as a count and value.
 *
 * @param {string} input plaintext
 * @return {string} ciphertext
 */
export const encode = (input) => {
  const encodedPair = (
    /** @type {string} */ match,
    /** @type {string} */ char
  ) => `${match.length}${char}`;

  return input.replace(/(.)\1+/g, encodedPair);
};

/**
 * Decode returns a string that has been reconstructed from the input into its original form.
 *
 * @param {string} input ciphertext
 * @return {string} plaintext
 */
export const decode = (input) => {
  const decodedGroup = (
    /** @type {string} */ _match,
    /** @type {number} */ count,
    /** @type {string} */ char
  ) => char.repeat(count);

  return input.replace(/(\d+)(.)/g, decodedGroup);
};
