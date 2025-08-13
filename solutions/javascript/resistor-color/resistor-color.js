//@ts-check
//
// Helpful program so that you don't have to remember the values of the bands.
//

/** Return the value of a color band
 *
 * @param {string} color
 * @return {number}
 */
export const colorCode = (color) => COLORS.indexOf(color);

// Better Be Right Or Your Great Big Values Go Wrong
export const COLORS = [
  "black",
  "brown",
  "red",
  "orange",
  "yellow",
  "green",
  "blue",
  "violet",
  "grey",
  "white",
];
