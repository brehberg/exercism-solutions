//@ts-check

//
// Determine the actions of a secret handshake based on the binary
// representation of the given `code`.
//

const ACTIONS = new Map(
  Object.entries({
    0b00001: "wink",
    0b00010: "double blink",
    0b00100: "close your eyes",
    0b01000: "jump",
    0b10000: "reverse",
  })
);

export const commands = (/** @type {number} */ code) => {
  const handshake = [];

  ACTIONS.forEach((action, key) => {
    if (!(code & Number(key))) return;
    if (action === "reverse") handshake.reverse();
    else handshake.push(action);
  });

  return handshake;
};
