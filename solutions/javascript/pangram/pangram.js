//@ts-check

/**
 * isPangram returns true if a word or sentence is a pangram. A pangram
 * is a sentence using every letter of the alphabet at least once.
 *
 * @param {string} phrase
 * @return {boolean}
 */
export const isPangram = (phrase) => {
  const letters = new Set();
  [...phrase.toLowerCase()].forEach((c) => {
    if (/[a-z]/.test(c)) letters.add(c);
  });
  return letters.size === 26;
};
