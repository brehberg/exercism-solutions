defmodule SquareRoot do
  @doc """
  Calculate the integer square root of a positive integer
  """
  @spec calculate(radicand :: pos_integer) :: pos_integer
  def calculate(radicand), do: do_calc(radicand, 0)

  @spec do_calc(non_neg_integer, non_neg_integer) :: pos_integer
  defp do_calc(0, step), do: step
  defp do_calc(n, step), do: do_calc(n - odd_value(step), step + 1)

  @spec odd_value(non_neg_integer) :: pos_integer
  defp odd_value(num), do: num * 2 + 1
end
