// @ts-check

/**
 * Returns all candidates that are anagrams of, but not equal to, 'target'.
 *
 * @param{string} target
 * @param{string[]} candidates
 *
 * @returns{string[]} anagrams
 */
export const findAnagrams = (target, candidates) => {
  const base = target.toLowerCase();
  const anagrams = [];

  const sorted = (/** @type {string} */ s) => s.split("").sort().join("");

  for (const candidate of candidates) {
    const word = candidate.toLowerCase();
    if (base === word) continue;
    if (sorted(base) !== sorted(word)) continue;
    anagrams.push(candidate);
  }
  return anagrams;
};
