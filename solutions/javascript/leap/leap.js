//
// This is only a SKELETON file for the 'Leap' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export const isLeap = (year) => {
  const divisibleBy4 = (year % 4 === 0);
  const divisibleBy100 = (year % 100 === 0);
  const divisibleBy400 = (year % 400 === 0);
  return (divisibleBy4 && divisibleBy400) || (divisibleBy4 && !divisibleBy100);
};
