"""Solution to Ellen's Alien Game exercise."""


class Alien:
    """Create an Alien object with location x_coordinate and y_coordinate.

    Attributes
    ----------
    (class)total_aliens_created: int
    x_coordinate: int - Position on the x-axis.
    y_coordinate: int - Position on the y-axis.
    health: int - Number of health points.

    Methods
    -------
    hit(): Decrement Alien health by one point.
    is_alive(): Return a boolean for if Alien is alive (if health is > 0).
    teleport(new_x_coordinate, new_y_coordinate): Move Alien object to new coordinates.
    collision_detection(other): Implementation TBD.
    """

    total_aliens_created = 0

    def __init__(self, x_pos, y_pos):
        self.x_coordinate = x_pos
        self.y_coordinate = y_pos
        self.health = 3
        Alien.total_aliens_created += 1

    def hit(self):
        self.health = health if (health := self.health - 1) > 0 else 0

    def is_alive(self):
        return self.health > 0

    def teleport(self, x_pos, y_pos):
        self.x_coordinate = x_pos
        self.y_coordinate = y_pos

    def collision_detection(self, other):
        pass


def new_aliens_collection(positions):
    return [Alien(x_pos, y_pos) for x_pos, y_pos in positions]
