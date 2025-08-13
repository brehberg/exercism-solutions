defmodule RobotSimulator do
  defstruct facing: :north, x: 0, y: 0
  defguard is_direction(direction) when direction in [:north, :east, :south, :west]
  defguard is_position(x, y) when is_integer(x) and is_integer(y)

  @type robot() :: %RobotSimulator{facing: direction(), x: integer(), y: integer()}
  @type direction() :: :north | :east | :south | :west
  @type position() :: {integer(), integer()}

  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec create(direction, position) :: robot() | {:error, String.t()}
  def create(), do: %RobotSimulator{}
  def create(dir, _) when not is_direction(dir), do: {:error, "invalid direction"}
  def create(dir, {x, y}) when is_position(x, y), do: %RobotSimulator{facing: dir, x: x, y: y}
  def create(_, _), do: {:error, "invalid position"}

  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot, instructions :: String.t()) :: robot() | {:error, String.t()}
  def simulate(robot, "R") do
    case robot.facing do
      :north -> %{robot | facing: :east}
      :east -> %{robot | facing: :south}
      :south -> %{robot | facing: :west}
      :west -> %{robot | facing: :north}
    end
  end

  def simulate(robot, "L") do
    case robot.facing do
      :north -> %{robot | facing: :west}
      :west -> %{robot | facing: :south}
      :south -> %{robot | facing: :east}
      :east -> %{robot | facing: :north}
    end
  end

  def simulate(robot, "A") do
    case robot.facing do
      :north -> %{robot | y: robot.y + 1}
      :west -> %{robot | x: robot.x - 1}
      :south -> %{robot | y: robot.y - 1}
      :east -> %{robot | x: robot.x + 1}
    end
  end

  def simulate(robot, <<first::binary-size(1), rest::binary>>) do
    case first in ["R", "L", "A"] do
      false -> {:error, "invalid instruction"}
      true -> robot |> simulate(first) |> simulate(rest)
    end
  end

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot) :: direction()
  def direction(robot), do: robot.facing

  @doc """
  Return the robot's position.
  """
  @spec position(robot) :: position()
  def position(robot), do: {robot.x, robot.y}
end
