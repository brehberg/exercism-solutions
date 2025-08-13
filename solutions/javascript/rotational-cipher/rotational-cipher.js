//@ts-check
//
// Implementats the rotational cipher, also sometimes called the Caesar cipher.
// Rotate returns a rotated string when given a plaintext and amount to shift by.
//

const lowerCaseStart = "a".charCodeAt(0);
const lowerCaseEnd = "z".charCodeAt(0);
const upperCaseStart = "A".charCodeAt(0);
const upperCaseEnd = "Z".charCodeAt(0);

export const rotate = (
  /** @type {string} */ plaintext,
  /** @type {number} */ shiftKey
) => {
  const cyphertext = [];

  for (let i = 0; i < plaintext.length; ++i) {
    const charCode = plaintext.charCodeAt(i);

    cyphertext.push(
      lowerCaseStart <= charCode && charCode <= lowerCaseEnd
        ? rotateChar(charCode, shiftKey, lowerCaseStart)
        : upperCaseStart <= charCode && charCode <= upperCaseEnd
        ? rotateChar(charCode, shiftKey, upperCaseStart)
        : charCode
    );
  }

  return String.fromCharCode(...cyphertext);
};

const rotateChar = (
  /** @type {number} */ code,
  /** @type {number} */ key,
  /** @type {number} */ start
) => {
  return start + ((code + key - start) % 26);
};
