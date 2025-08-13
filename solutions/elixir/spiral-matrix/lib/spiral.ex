defmodule Spiral do
  defstruct matrix: [[]]

  @type value :: %{
          :last => integer,
          :done => integer
        }

  @type position :: %{
          :row => integer,
          :col => integer,
          :mix => integer,
          :max => integer
        }

  @doc """
  Given the dimension, return a square matrix of numbers in clockwise spiral order.
  """
  @spec matrix(dimension :: integer) :: list(list(integer))
  def matrix(dimension) do
    0
    |> List.duplicate(dimension)
    |> List.duplicate(dimension)
    |> go_right(
      %{last: 0, done: dimension * dimension},
      %{row: 0, col: 0, min: -1, max: dimension}
    )
  end

  @spec go_right(%Spiral{}, value, position) :: %Spiral{}
  defp go_right(spiral, %{last: n, done: n}, _), do: spiral

  # position beyond last column, turn down instead
  defp go_right(spiral, val, %{row: r, col: c, max: c} = pos) do
    spiral
    |> go_down(val, %{pos | row: r + 1, col: c - 1})
  end

  # update this position and move right to next column
  defp go_right(spiral, %{last: n} = val, %{row: r, col: c} = pos) do
    update(spiral, r, c, n + 1)
    |> go_right(%{val | last: n + 1}, %{pos | col: c + 1})
  end

  @spec go_down(%Spiral{}, value, position) :: %Spiral{}
  defp go_down(spiral, %{last: n, done: n}, _), do: spiral

  # position beyond last row, turn left instead
  defp go_down(spiral, val, %{row: r, col: c, max: r} = pos) do
    spiral
    |> go_left(val, %{pos | row: r - 1, col: c - 1})
  end

  # update this position and move down to next row
  defp go_down(spiral, %{last: n} = val, %{row: r, col: c} = pos) do
    update(spiral, r, c, n + 1)
    |> go_down(%{val | last: n + 1}, %{pos | row: r + 1})
  end

  @spec go_left(%Spiral{}, value, position) :: %Spiral{}
  defp go_left(spiral, %{last: n, done: n}, _), do: spiral

  # position beyond first column, turn up and increase min position
  defp go_left(spiral, val, %{row: r, col: c, min: c} = pos) do
    spiral
    |> go_up(val, %{pos | row: r - 1, col: c + 1, min: c + 1})
  end

  # update this position and move left to previous column
  defp go_left(spiral, %{last: n} = val, %{row: r, col: c} = pos) do
    update(spiral, r, c, n + 1)
    |> go_left(%{val | last: n + 1}, %{pos | col: c - 1})
  end

  @spec go_up(%Spiral{}, value, position) :: %Spiral{}
  defp go_up(spiral, %{last: n, done: n}, _), do: spiral

  # position beyond first row, turn right and decrease max position
  defp go_up(spiral, val, %{row: r, col: c, min: r, max: m} = pos) do
    spiral
    |> go_right(val, %{pos | row: r + 1, col: c + 1, max: m - 1})
  end

  # update this position and move up to previous row
  defp go_up(spiral, %{last: n} = val, %{row: r, col: c} = pos) do
    update(spiral, r, c, n + 1)
    |> go_up(%{val | last: n + 1}, %{pos | row: r - 1})
  end

  @spec update(%Spiral{}, integer, integer, integer) :: %Spiral{}
  defp update(spiral, row, col, val) do
    spiral |> List.replace_at(row, update_row(spiral, row, col, val))
  end

  @spec update_row(%Spiral{}, integer, integer, integer) :: [integer]
  defp update_row(spiral, row, col, val) do
    spiral |> Enum.at(row) |> List.replace_at(col, val)
  end
end
