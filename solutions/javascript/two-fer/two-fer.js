/**
 * Determine what you will say as you give away the extra cookie.
 *
 * @param {string} [name] of your friend that likes cookies
 *
 * @returns {string} response
 */
export const twoFer = (name = "you") => {
  return `One for ${name}, one for me.`;
};
