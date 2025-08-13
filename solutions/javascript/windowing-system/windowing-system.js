// @ts-check

/**
 * Define Size for storing the dimensions of the window
 *
 * @param {number} [width] of the window
 * @param {number} [height] of the window
 */
export function Size(width = 80, height = 60) {
  this.width = width;
  this.height = height;
}

Size.prototype.resize = function (
  /** @type {number} */ newWidth,
  /** @type {number} */ newHeight
) {
  this.width = newWidth;
  this.height = newHeight;
};

/**
 * Define Position to store a window position
 *
 * @param {number} [x] horizontal position
 * @param {number} [y] vertical position
 */
export function Position(x = 0, y = 0) {
  this.x = x;
  this.y = y;
}

Position.prototype.move = function (
  /** @type {number} */ newX,
  /** @type {number} */ newY
) {
  this.x = newX;
  this.y = newY;
};

export class ProgramWindow {
  constructor() {
    this.screenSize = new Size(800, 600);
    this.size = new Size();
    this.position = new Position();
  }

  /**
   * Add a method to resize the window
   *
   * @param {Size} newSize of the window
   */
  resize(newSize) {
    // The minimum allowed height or width is 1.
    this.size.width = newSize.width < 1 ? 1 : newSize.width;
    this.size.height = newSize.height < 1 ? 1 : newSize.height;

    // The maximum height and width depend on the position of the window,
    // the edges of the window cannot move past the edges of the screen.
    if (this.size.width + this.position.x > this.screenSize.width) {
      this.size.width = this.screenSize.width - this.position.x;
    }
    if (this.size.height + this.position.y > this.screenSize.height) {
      this.size.height = this.screenSize.height - this.position.y;
    }
  }

  /**
   * Add a method to move the window
   *
   * @param {Position} newPosition of the window
   */
  move(newPosition) {
    // The smallest position is 0 for both x and y.
    this.position.x = newPosition.x < 0 ? 0 : newPosition.x;
    this.position.y = newPosition.y < 0 ? 0 : newPosition.y;

    // The maximum position in either direction depends on the size of
    // the window. The edges cannot move past the edges of the screen.
    this.position.x = Math.max(
      this.position.x,
      this.screenSize.width - this.size.width
    );
    this.position.y = Math.max(
      this.position.y,
      this.screenSize.height - this.size.height
    );
  }
}

/**
 * Change a program window
 *
 * @param {ProgramWindow} programWindow to be changed
 *
 * @returns {ProgramWindow} instance after the changes were applied
 */
export function changeWindow(programWindow) {
  programWindow.move(new Position(100, 150));
  programWindow.resize(new Size(400, 300));
  return programWindow;
}
