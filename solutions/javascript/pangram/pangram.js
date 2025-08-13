//
// isPangram returns true if a word or sentence is a pangram. A pangram
// is a sentence using every letter of the alphabet at least once.
//
export const isPangram = (phrase) => {
  let letters = new Set();
  [...phrase.toLowerCase()].forEach((c) => {
    if (c.match(/[a-z]/)) letters.add(c);
  });
  return letters.size === 26;
};
