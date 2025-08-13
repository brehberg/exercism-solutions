// @ts-check
/**
 * Convert a phrase to its acronym.
 *
 * @param {String} phrase
 * @returns {String} acronym
 */
export const parse = (phrase) =>
  phrase.replace(/(?<!_)\B[\w']+|[\s\W_]/g, "").toUpperCase();
