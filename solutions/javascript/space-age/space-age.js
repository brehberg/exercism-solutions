// @ts-check

/**
 * @typedef {'mercury' |
 *           'venus' |
 *           'earth' |
 *           'mars' |
 *           'jupiter' |
 *           'saturn' |
 *           'uranus' |
 *           'neptune'}  Planet;
 */

/**
 * Given a planet and an age in seconds, calculate how old someone would be
 *
 * @param {Planet} planet
 * @param {number} seconds in seconds
 * @returns {number} age in years on planet
 */
export const age = (planet, seconds) => {
  /**
   * @type {Record<Planet,number>}
   */
  const ORBITAL_PERIOD = {
    mercury: 0.2408467,
    venus: 0.61519726,
    earth: 1.0,
    mars: 1.8808158,
    jupiter: 11.862615,
    saturn: 29.447498,
    uranus: 84.016846,
    neptune: 164.79132,
  };
  const EARTH_YEAR = 31557600; // seconds per earth year
  return Number((seconds / EARTH_YEAR / ORBITAL_PERIOD[planet]).toFixed(2));
};
