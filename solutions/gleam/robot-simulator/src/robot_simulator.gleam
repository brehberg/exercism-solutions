pub type Robot {
  Robot(direction: Direction, position: Position)
}

pub type Direction {
  North
  East
  South
  West
}

pub type Position {
  Position(x: Int, y: Int)
}

pub fn create(direction: Direction, position: Position) -> Robot {
  Robot(direction, position)
}

pub fn move(
  direction: Direction,
  position: Position,
  instructions: String,
) -> Robot {
  create(direction, position) |> simulate(instructions)
}

fn simulate(robot: Robot, instructions: String) -> Robot {
  case instructions {
    "R" <> rest -> robot |> turn_right() |> simulate(rest)
    "L" <> rest -> robot |> turn_left() |> simulate(rest)
    "A" <> rest -> robot |> advance() |> simulate(rest)
    _ -> robot
  }
}

fn turn_right(robot: Robot) -> Robot {
  case robot.direction {
    North -> Robot(..robot, direction: East)
    East -> Robot(..robot, direction: South)
    South -> Robot(..robot, direction: West)
    West -> Robot(..robot, direction: North)
  }
}

fn turn_left(robot: Robot) -> Robot {
  case robot.direction {
    North -> Robot(..robot, direction: West)
    East -> Robot(..robot, direction: North)
    South -> Robot(..robot, direction: East)
    West -> Robot(..robot, direction: South)
  }
}

fn advance(robot: Robot) -> Robot {
  let pos = robot.position
  case robot.direction {
    North -> Robot(..robot, position: Position(..pos, y: pos.y + 1))
    East -> Robot(..robot, position: Position(..pos, x: pos.x + 1))
    South -> Robot(..robot, position: Position(..pos, y: pos.y - 1))
    West -> Robot(..robot, position: Position(..pos, x: pos.x - 1))
  }
}
