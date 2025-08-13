defmodule School do
  @moduledoc """
  Simulate students in a school.

  Each student is in a grade.
  """

  @type school :: %{(grade :: integer) => names :: [String.t()]}

  @doc """
  Create a new, empty school.
  """
  @spec new() :: school
  def new(), do: %{}

  @doc """
  Add a student to a particular grade in school.
  """
  @spec add(school, String.t(), integer) :: {:ok | :error, school}
  def add(school, name, grade) do
    case name in roster(school) do
      true -> {:error, school}
      _ -> {:ok, Map.update(school, grade, [name], &[name | &1])}
    end
  end

  @doc """
  Return the names of the students in a particular grade, sorted alphabetically.
  """
  @spec grade(school, integer) :: [String.t()]
  def grade(school, grade) do
    case school[grade] do
      nil -> []
      names -> Enum.sort(names)
    end
  end

  @doc """
  Return the names of all the students in the school sorted by grade and name.
  """
  @spec roster(school) :: [String.t()]
  def roster(school) do
    school
    |> Map.keys()
    |> Enum.sort()
    |> Enum.map(&grade(school, &1))
    |> List.flatten()
  end
end
