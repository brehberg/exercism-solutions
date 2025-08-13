export const toRoman = (num: number): string => {
  const _letters = "MDCLXVI";  // 1000  500  100  50  10  5  1

  let index = 0,
    result = "",
    value = 1000,
    radix = 2,
    subtractValue = 0;

  while (num > 0) {
    while (num >= value) {
      result += _letters[index];
      num -= value;
    }

    if (radix == 2) {
      subtractValue = value / 10;  // will be 100  10  1
    }

    if (num + subtractValue >= value) { // add for 4 and 9
      result += _letters[index + 1 + Number(radix == 2)];
      num += subtractValue;
      continue;
    }

    index += 1;      // move term index to point to next letter
    value /= radix;  // set value with divide by either 2 or 5
    radix ^= 7;      // swap between 2 and 5 for next pass value
  }
  return result
}
