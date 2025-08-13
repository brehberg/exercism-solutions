//
// Checks that all the brackets and braces in the string are matched correctly, and nested correctly
//
const matches = {
  "[": "]",
  "{": "}",
  "(": ")",
};

export const isPaired = (input) => {
  const closerNeeded = [];
  for (const c of input) {
    if (Object.values(matches).includes(c)) {
      // closing bracket was found, is it the next expected value on stack?
      if (closerNeeded.pop() != c) return false;
    } else if (Object.keys(matches).includes(c)) {
      // opening bracket was found, add matching closing value to the stack
      closerNeeded.push(matches[c]);
    }
  }
  return closerNeeded.length === 0;
};
