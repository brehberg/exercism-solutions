//@ts-check
export class ZebraPuzzle {
  constructor() {
    this.residents = ["Unable to find solution!"];

    for (const residents of permute(NATIONS)) {
      // 10. The Norwegian lives in the first house.
      if (residents[FIRST_HOUSE] !== "Norwegian") continue;

      for (const houses of permute(COLORS)) {
        // 2. The Englishman lives in the red house.
        if (houses[residents.indexOf("Englishman")] !== "red") continue;
        // 15. The Norwegian lives next to the blue house.
        if (!neighbors(houses, "blue", residents, "Norwegian")) continue;
        // 6. The green house is immediately to the right of the ivory house.
        if (houses[houses.indexOf("ivory") + 1] !== "green") continue;

        for (const hobbies of permute(HOBBIES)) {
          // 8. The person in the yellow house is a painter.
          if (hobbies[houses.indexOf("yellow")] !== "painting") continue;
          // 14. The Japanese person plays chess.
          if (hobbies[residents.indexOf("Japanese")] !== "chess") continue;

          for (const pets of permute(ANIMALS)) {
            // 11. The person who enjoys reading lives in the house next to the person with the fox.
            if (!neighbors(pets, "fox", hobbies, "reading")) continue;
            // 12. The painter's house is next to the house with the horse.
            if (!neighbors(pets, "horse", hobbies, "painting")) continue;
            // 3. The Spaniard owns the dog.
            if (pets[residents.indexOf("Spaniard")] !== "dog") continue;
            // 7. The snail owner likes to go dancing.
            if (pets[hobbies.indexOf("dancing")] !== "snail") continue;

            for (const drinks of permute(DRINKS)) {
              // 9. The person in the middle house drinks milk.
              if (drinks[MIDDLE_HOUSE] !== "milk") continue;
              // 5. The Ukrainian drinks tea.
              if (drinks[residents.indexOf("Ukrainian")] !== "tea") continue;
              // 4. The person in the green house drinks coffee.
              if (drinks[houses.indexOf("green")] !== "coffee") continue;
              // 13. The person who plays football drinks orange juice.
              if (drinks[hobbies.indexOf("football")] !== "juice") continue;

              // All rules have passed and a solution has been found...
              this.residents = residents;
              this.houses = houses;
              this.hobbies = hobbies;
              this.pets = pets;
              this.drinks = drinks;
              return;
            }
          }
        }
      }
    }
  }
  waterDrinker = () => this.residents[this.drinks?.indexOf("water") ?? 0];
  zebraOwner = () => this.residents[this.pets?.indexOf("zebra") ?? 0];
}

const NATIONS = [
  "Englishman",
  "Spaniard",
  "Ukrainian",
  "Norwegian",
  "Japanese",
];
const HOBBIES = ["chess", "football", "dancing", "painting", "reading"];
const ANIMALS = ["dog", "snail", "fox", "horse", "zebra"];
const COLORS = ["red", "green", "ivory", "yellow", "blue"];
const DRINKS = ["coffee", "milk", "juice", "tea", "water"];

const FIRST_HOUSE = 0;
const MIDDLE_HOUSE = 2;

const neighbors = (
  /** @type {string[]} */ arr1,
  /** @type {string} */ key1,
  /** @type {string[]} */ arr2,
  /** @type {string} */ key2
) => Math.abs(arr1.indexOf(key1) - arr2.indexOf(key2)) === 1;

// use Heap's method to generate all permutations of elements
// taken from https://stackoverflow.com/a/37580979
function* permute(/** @type {any[]} */ elements) {
  var length = elements.length,
    c = Array(length).fill(0),
    i = 1,
    k,
    p;

  yield elements.slice();
  while (i < length) {
    if (c[i] < i) {
      k = i % 2 && c[i];
      p = elements[i];
      elements[i] = elements[k];
      elements[k] = p;
      c[i] += 1;
      i = 1;
      yield elements.slice();
    } else {
      c[i] = 0;
      i += 1;
    }
  }
}
