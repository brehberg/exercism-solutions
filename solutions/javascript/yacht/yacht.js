// @ts-check

/**
 * @typedef {'ones' |
 *           'twos' |
 *           'threes' |
 *           'fours' |
 *           'fives' |
 *           'sixes' |
 *           'full house' |
 *           'four of a kind' |
 *           'little straight' |
 *           'big straight' |
 *           'choice' |
 *           'yacht' }  Category;
 */

/**
 * Score a single throw of dice in the game Yacht.
 *
 * @param {number[]} dice - always five dice
 * @param {Category} category for scoring
 * @return {number} score
 */
export const score = (dice, category) => {
  const checkYacht = () => (new Set(dice).size === 1 ? 50 : 0);

  const checkStraight = (/** @type {number} */ start) => {
    dice.sort((a, b) => a - b);
    for (const die of dice) if (die !== start++) return 0;
    // worth 30 points if all required dice values are present
    return 30;
  };

  const checkFourOfAKind = () => {
    const groupsOfDice = determineGroupsOfDice();
    if (groupsOfDice.length > 2) return 0;
    groupsOfDice.sort((a, b) => b.count - a.count);
    return groupsOfDice[0].count >= 4 ? groupsOfDice[0].key * 4 : 0;
  };

  const checkFullHouse = () => {
    const groupsOfDice = determineGroupsOfDice();
    if (groupsOfDice.length !== 2) return 0;
    groupsOfDice.sort((a, b) => b.count - a.count);
    return groupsOfDice[0].count === 3 ? sumAll() : 0;
  };

  const sumAll = () => dice.reduce((sum, die) => (sum += die), 0);

  const sumForValue = (/** @type {number} */ value) =>
    dice.reduce((sum, die) => (sum += die === value ? die : 0), 0);

  /** @typedef {{key: number, count: number}} DiceGroup */
  const determineGroupsOfDice = () =>
    dice.reduce((/** @type {DiceGroup[]} */ result, die) => {
      const group = result.find((g) => g.key === die);
      if (group) group.count += 1;
      else result.push({ key: die, count: 1 });
      return result;
    }, []);

  switch (category) {
    case "ones":
      return sumForValue(1);
    case "twos":
      return sumForValue(2);
    case "threes":
      return sumForValue(3);
    case "fours":
      return sumForValue(4);
    case "fives":
      return sumForValue(5);
    case "sixes":
      return sumForValue(6);
    case "full house":
      return checkFullHouse();
    case "four of a kind":
      return checkFourOfAKind();
    case "little straight":
      return checkStraight(1);
    case "big straight":
      return checkStraight(2);
    case "choice":
      return sumAll();
    case "yacht":
      return checkYacht();
    default:
      return 0;
  }
};
