//@ts-check

/**
 * Compute a word's Scrabble score by summing the values of its letters.
 *
 * @param {string} input
 * @returns {number}
 */
export const score = (input) => {
  let sum = input
    .replace(/[^A-Z]/gi, "")
    .replace(/[AEIOULNRST]/gi, "+1")
    .replace(/[DG]/gi, "+2")
    .replace(/[BCMP]/gi, "+3")
    .replace(/[FHVWY]/gi, "+4")
    .replace(/[K]/gi, "+5")
    .replace(/[JX]/gi, "+8")
    .replace(/[QZ]/gi, "+10");

  return eval("0" + sum);
};
