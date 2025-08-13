//@ts-check

/**
 * Encode returns a string where consecutive elements are represented as a count and value.
 *
 * @param {string} input plaintext
 * @return {string} ciphertext
 */
export const encode = (input) => {
  const encode_pairs = (
    /** @type {string} */ match,
    /** @type {string} */ p1
  ) => {
    return `${match.length}${p1}`;
  };

  return input.replace(/(.)\1+/g, encode_pairs);
};

/**
 * Decode returns a string that has been reconstructed from the input into its original form.
 *
 * @param {string} input ciphertext
 * @return {string} plaintext
 */
export const decode = (input) => {
  const decode_groups = (
    /** @type {string} */ _match,
    /** @type {number} */ p1,
    /** @type {string} */ p2
  ) => {
    return p2.repeat(p1);
  };

  return input.replace(/(\d+)(.)/g, decode_groups);
};
