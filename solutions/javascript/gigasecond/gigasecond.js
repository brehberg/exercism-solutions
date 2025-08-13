//
// Calculate a date one billion seconds after an input date.
//
const MILLISEC_PER_GIGASECOND = 1e12;

export const gigasecond = (input) => {
  return new Date(input.getTime() + MILLISEC_PER_GIGASECOND);
};
