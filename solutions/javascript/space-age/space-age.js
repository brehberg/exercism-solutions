/**
 * Given a planet and an age in seconds, calculate how old someone would be
 */
export const age = (planet, seconds) => {
  /**
   * @type{Record<string,number>}
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
  return Math.round((seconds / 31557600 / ORBITAL_PERIOD[planet]) * 100) / 100;
};
