//@ts-check
//
// Helpful program so that you don't have to remember the values of the bands.
//

/** Calculate a resistance value from two colors
 *
 * @param {string[]} colors
 * @return {number}
 */
export const decodedValue = (colors) =>
  COLORS.indexOf(colors[0]) * 10 + COLORS.indexOf(colors[1]);

const COLORS = [
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
