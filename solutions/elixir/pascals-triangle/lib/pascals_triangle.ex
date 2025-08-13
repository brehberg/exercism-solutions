defmodule PascalsTriangle do
  @doc """
  Calculates the rows of a pascal triangle
  with the given height
  """
  @spec rows(height :: integer) :: [[integer]]
  def rows(num), do: do_compute(num - 1, [[1]])

  @spec do_compute(remaining :: integer, current :: [[integer]]) :: [[integer]]
  defp do_compute(0, final), do: final

  defp do_compute(n, rows) do
    do_compute(n - 1, rows ++ [List.last(rows) |> next_row()])
  end

  @spec next_row([integer]) :: [integer]
  defp next_row(previous_row) do
    [1 | Enum.chunk_every(previous_row, 2, 1) |> Enum.map(&Enum.sum/1)]
  end
end
