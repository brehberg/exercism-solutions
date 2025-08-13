export class Squares {
  constructor(max) {
    this._n = max;
  }

  get sumOfSquares() {
    return (this._n + 3 * this._n ** 2 + 2 * this._n ** 3) / 6;
  }

  get squareOfSum() {
    return ((this._n + this._n ** 2) / 2) ** 2;
  }

  get difference() {
    return this.squareOfSum - this.sumOfSquares;
  }
}
