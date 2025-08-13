/**
 * Allergies
 *
 * Given a person's allergy score, determine whether or not they're allergic
 * to a given item, and their full list of allergies.
 */
const ALLERGENS = [
  "eggs",
  "peanuts",
  "shellfish",
  "strawberries",
  "tomatoes",
  "chocolate",
  "pollen",
  "cats",
];

export class Allergies {
  constructor(score) {
    this.score = score;
  }

  /**
   * Give the whole list of allergies.
   * @returns {string[]} List of allergies.
   */
  list() {
    return ALLERGENS.filter((_, i) => this.score & (1 << i));
  }

  /**
   * Determine whether the user is allergic or not to some element.
   * @param {string} allergen Allergy type.
   * @returns {boolean} True if allergic to the given type.
   */
  allergicTo(allergen) {
    return this.list().includes(allergen);
  }
}
