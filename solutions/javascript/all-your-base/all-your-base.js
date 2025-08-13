export const convert = (fromDigits, fromBase, toBase) => {
  const errorWrongFormat = new Error("Input has wrong format");
  const errorInputBase = new Error("Wrong input base");
  const errorOutputBase = new Error("Wrong output base");

  if (fromBase < 2) throw errorInputBase;
  if (toBase < 2) throw errorOutputBase;
  if (!fromDigits.length) throw errorWrongFormat;
  if (!fromDigits[0] && fromDigits.length > 1) throw errorWrongFormat;

  // convert sequence of digits in input base to whole integer value
  const positionalValue = (acc, d) => {
    if (d < 0 || d >= fromBase) throw errorWrongFormat;
    acc *= fromBase;
    acc += d;
    return acc;
  };
  let value = fromDigits.reduce(positionalValue, 0);

  // convert whole integer value to sequence of digits in output base
  const toDigits = [];
  while (value >= toBase) {
    toDigits.unshift(value % toBase);
    value = Math.floor(value / toBase);
  }
  toDigits.unshift(value);

  return toDigits;
};
