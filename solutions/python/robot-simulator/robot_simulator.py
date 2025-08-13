"""A robot factory's test facility program to verify robot movements."""

# Globals for the directions used as indices when accessing movement lists
NORTH = 0
EAST = 1
SOUTH = 2
WEST = 3

TURN_RIGHT = [EAST, SOUTH, WEST, NORTH]
TURN_LEFT = [WEST, NORTH, EAST, SOUTH]
ADVANCE = [(0, 1), (1, 0), (0, -1), (-1, 0)]


class Robot:
    """Robots are placed on a hypothetical infinite grid, facing a particular
    direction (north, east, south, or west) at a set of {x,y} coordinates"""

    def __init__(self, direction=NORTH, x_pos=0, y_pos=0):
        self.direction = direction
        self.coordinates = (x_pos, y_pos)

    def move(self, instructions: str):
        """The robots have three possible movements:
        R = turn right, L = turn left, A = advance"""
        for inst in instructions:
            match inst:
                case "R":
                    self.direction = TURN_RIGHT[self.direction]
                case "L":
                    self.direction = TURN_LEFT[self.direction]
                case "A":
                    self.coordinates = tuple(
                        map(sum, zip(self.coordinates, ADVANCE[self.direction]))
                    )
