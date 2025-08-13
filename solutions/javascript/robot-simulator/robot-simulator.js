//
// A robot factory's test facility program to verify robot movements.
//
export class InvalidInputError extends Error {
  constructor(message) {
    super();
    this.message = message || "Invalid Input";
  }
}

const DIRECTIONS = ["north", "east", "south", "west"];

export class Robot {
  #facing = "north";
  #x = 0;
  #y = 0;

  get bearing() {
    return this.#facing;
  }

  get coordinates() {
    return [this.#x, this.#y];
  }

  place({ x, y, direction }) {
    if (!DIRECTIONS.includes(direction)) {
      throw new InvalidInputError("Invalid direction!");
    }
    this.#facing = direction;
    this.#x = x;
    this.#y = y;
  }

  evaluate(instructions) {
    [...instructions].forEach((inst) => {
      switch (inst) {
        case "R":
          this.#turn_right();
          break;
        case "L":
          this.#turn_left();
          break;
        case "A":
          this.#advance();
          break;
      }
    });
  }

  #turn_right() {
    switch (this.#facing) {
      case "north":
        this.#facing = "east";
        break;
      case "east":
        this.#facing = "south";
        break;
      case "south":
        this.#facing = "west";
        break;
      case "west":
        this.#facing = "north";
        break;
    }
  }

  #turn_left() {
    switch (this.#facing) {
      case "north":
        this.#facing = "west";
        break;
      case "east":
        this.#facing = "north";
        break;
      case "south":
        this.#facing = "east";
        break;
      case "west":
        this.#facing = "south";
        break;
    }
  }

  #advance() {
    switch (this.#facing) {
      case "north":
        this.#y += 1;
        break;
      case "east":
        this.#x += 1;
        break;
      case "south":
        this.#y -= 1;
        break;
      case "west":
        this.#x -= 1;
        break;
    }
  }
}
