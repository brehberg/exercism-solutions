//
// Given a positive number, convert it into roman numeral string representation.
//
export const toRoman = (number) => {
  if (number < 1 || number > 3999) {
    throw new Error("Number has to be positive integer in range of 1-3999.");
  }

  const ones = ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"];
  const tens = ["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"];
  const hund = ["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"];
  const thou = ["", "M", "MM", "MMM"];

  return (
    thou[Math.floor(number / 1000)] +
    hund[Math.floor((number % 1000) / 100)] +
    tens[Math.floor((number % 100) / 10)] +
    ones[number % 10]
  );
};
