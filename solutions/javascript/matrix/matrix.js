export class Matrix {
  constructor(input) {
    this.matrix = input.split(/\r?\n/).map((line) => line.split(/\s+/).map(Number))
  }

  get rows() {
    return this.matrix;
  }

  get columns() {
    return this.matrix[0].map((_,colIndex) => this.matrix.map(row => row[colIndex]));
  }
}
