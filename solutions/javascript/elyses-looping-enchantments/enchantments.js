// @ts-check

/**
 * Determine how many cards of a certain type there are in the deck
 *
 * @param {number[]} stack
 * @param {number} card
 *
 * @returns {number} number of cards of a single type there are in the deck
 */
export function cardTypeCheck(stack, card) {
  let count = 0;
  stack.forEach((c) => (count += Number(c === card)));
  return count;
}

/**
 * Determine how many cards are odd or even
 *
 * @param {number[]} stack
 * @param {boolean} type the type of value to check for - odd or even
 *
 * @returns {number} number of cards that are either odd or even (depending on `type`)
 */
export function determineOddEvenCards(stack, type) {
  let count = 0;
  // type true is analogous to 'even', and false is analogous to 'odd'
  for (const card of stack) {
    count += Number(type ? isEven(card) : isOdd(card));
  }
  return count;
}

// Predicates to determine if card is odd or even
const isOdd = (/** @type {number} */ n) => n % 2;
const isEven = (/** @type {number} */ n) => !(n % 2);
