export const convert = (from_digits, from_base, to_base) => {
  const errorWrongFormat = new Error("Input has wrong format");
  const errorInputBase = new Error("Wrong input base");
  const errorOutputBase = new Error("Wrong output base");

  if (from_base < 2) throw errorInputBase;
  if (to_base < 2) throw errorOutputBase;
  if (!from_digits.length) throw errorWrongFormat;
  if (!from_digits[0] && from_digits.length > 1) throw errorWrongFormat;

  // convert sequence of digits in input base to whole integer value
  const positionOffset = from_digits.length - 1;
  const positionalValue = (sum, d, i) => {
    if (d < 0 || d >= from_base) throw errorWrongFormat;
    return sum + d * from_base ** (positionOffset - i);
  };
  let value = from_digits.reduce(positionalValue, 0);

  // convert whole integer value to sequence of digits in output base
  const to_digits = [];
  while (value >= to_base) {
    to_digits.unshift(value % to_base);
    value = Math.floor(value / to_base);
  }
  to_digits.unshift(value);

  return to_digits;
};
