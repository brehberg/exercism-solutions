//@ts-check

/**
 * isIsogram returns true if a word or sentence is an isogram.
 * An isogram is a word or phrase without a repeating letter.
 *
 * @param {string} phrase
 * @return {boolean}
 */
export const isIsogram = (phrase) => {
  const letters = new Set();
  for (const c of phrase.toLowerCase()) {
    if (/[a-z]/.test(c)) {
      if (letters.has(c)) return false;
      letters.add(c);
    }
  }
  return true;
};
