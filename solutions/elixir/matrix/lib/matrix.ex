defmodule Matrix do
  @moduledoc false
  defstruct matrix: [[]]

  @doc """
  Convert an `input` string, with rows separated by newlines and values
  separated by single spaces, into a `Matrix` struct.
  """
  @spec from_string(input :: String.t()) :: %Matrix{}
  def from_string(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&String.trim/1)
    |> Enum.map(&split_to_integer/1)
  end

  @doc false
  @spec split_to_integer(String.t()) :: [integer]
  defp split_to_integer(line),
    do: line |> String.split() |> Enum.map(&String.to_integer/1)

  @doc """
  Write the `matrix` out as a string, with rows separated by newlines and
  values separated by single spaces.
  """
  @spec to_string(matrix :: %Matrix{}) :: String.t()
  def to_string(matrix), do: matrix |> Enum.map_join("\n", &row_to_string/1)

  @doc false
  @spec row_to_string([integer]) :: String.t()
  defp row_to_string(row), do: row |> Enum.map_join("\s", & &1)

  @doc """
  Given a `matrix`, return its rows as a list of lists of integers.
  """
  @spec rows(matrix :: %Matrix{}) :: list(list(integer))
  def rows(matrix) do
    matrix
  end

  @doc """
  Given a `matrix` and `index`, return the row at `index`.
  """
  @spec row(matrix :: %Matrix{}, index :: integer) :: list(integer)
  def row(matrix, index) do
    matrix |> Enum.at(index - 1)
  end

  @doc """
  Given a `matrix`, return its columns as a list of lists of integers.
  """
  @spec columns(matrix :: %Matrix{}) :: list(list(integer))
  def columns(matrix) do
    1..length(matrix) |> Enum.map(&column(matrix, &1))
  end

  @doc """
  Given a `matrix` and `index`, return the column at `index`.
  """
  @spec column(matrix :: %Matrix{}, index :: integer) :: list(integer)
  def column(matrix, index) do
    matrix |> Enum.map(&Enum.at(&1, index - 1))
  end
end
