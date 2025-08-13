//
// Random character generator for a game of Dungeons & Dragons
//
export const abilityModifier = (score) => {
  if (score < 3) throw new Error("Ability scores must be at least 3");
  if (score > 18) throw new Error("Ability scores can be at most 18");
  return Math.floor(score / 2 - 5);
};

export class Character {
  #strength;
  #dexterity;
  #constitution;
  #intelligence;
  #wisdom;
  #charisma;
  #hitpoints;

  static rollAbility() {
    const rolls = [
      Character.rollDie(6),
      Character.rollDie(6),
      Character.rollDie(6),
      Character.rollDie(6),
    ];

    let sum = 0,
      min = Infinity;

    [...rolls].forEach((die) => {
      sum += die;
      min = Math.min(min, die);
    });
    return sum - min;
  }

  static rollDie(max) {
    return Math.floor(Math.random() * max + 1);
  }

  constructor() {
    this.#strength = Character.rollAbility();
    this.#dexterity = Character.rollAbility();
    this.#constitution = Character.rollAbility();
    this.#intelligence = Character.rollAbility();
    this.#wisdom = Character.rollAbility();
    this.#charisma = Character.rollAbility();
    this.#hitpoints = 10 + abilityModifier(this.#constitution);
  }

  get strength() {
    return this.#strength;
  }

  get dexterity() {
    return this.#dexterity;
  }

  get constitution() {
    return this.#constitution;
  }

  get intelligence() {
    return this.#intelligence;
  }

  get wisdom() {
    return this.#wisdom;
  }

  get charisma() {
    return this.#charisma;
  }

  get hitpoints() {
    return this.#hitpoints;
  }
}
