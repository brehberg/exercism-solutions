export const isLeap = (year) => {
  let isDivisibleBy = (n) => year % n === 0;
  return (isDivisibleBy(4) && !isDivisibleBy(100)) || isDivisibleBy(400);
};
