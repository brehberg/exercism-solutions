export class InvalidInputError extends Error {
  constructor(message: string) {
    super()
    this.message = message || 'Invalid Input'
  }
}

type Coordinates = [number, number]
const direction = ['north', 'east', 'south', 'west'] as const;
type Direction = (typeof direction)[number];
const isDirection = (x: any): x is Direction => direction.includes(x);

export class Robot {
  private facing: Direction = 'north';
  private x: number = 0;
  private y: number = 0;

  get bearing(): Direction {
    return this.facing;
  }

  get coordinates(): Coordinates {
    return [this.x, this.y];
  }

  place(input: { x: number; y: number; direction: string }) {
    if (!isDirection(input.direction)) {
      throw new InvalidInputError("invalid direction");
    }
    this.facing = <Direction>input.direction;
    this.x = input.x;
    this.y = input.y;
  }

  evaluate(instructions: string) {
    [...instructions].forEach((inst) => {
      switch (inst) {
        case 'R': this.turn_right(); break;
        case 'L': this.turn_left(); break;
        case 'A': this.advance(); break;
        default: break;
      }
    });
  }

  private turn_right() {
    switch (this.facing) {
      case 'north': this.facing = 'east'; break;
      case 'east': this.facing = 'south'; break;
      case 'south': this.facing = 'west'; break;
      case 'west': this.facing = 'north'; break;
    }
  }

  private turn_left() {
    switch (this.facing) {
      case 'north': this.facing = 'west'; break;
      case 'east': this.facing = 'north'; break;
      case 'south': this.facing = 'east'; break;
      case 'west': this.facing = 'south'; break;
    }
  }
  private advance() {
    switch (this.facing) {
      case 'north': this.y += 1; break;
      case 'east': this.x += 1; break;
      case 'south': this.y -= 1; break;
      case 'west': this.x -= 1; break;
    }
  }
}
