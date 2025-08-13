/**
 * Determine the RNA complement of a given DNA sequence.
 *
 * @param {string} dna
 * @returns {string}
 */
export const toRna = (dna) => {
  /**
   * @type {Record<string,string}
   */
  const DNA2RNA = {
    G: "C",
    C: "G",
    T: "A",
    A: "U",
  };
  return dna
    .split("")
    .map((nucleotide) => DNA2RNA[nucleotide])
    .join("");
};
