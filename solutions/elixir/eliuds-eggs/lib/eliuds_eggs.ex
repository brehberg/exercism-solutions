defmodule EliudsEggs do
  import Bitwise

  @doc """
  Given the number, count the number of eggs.
  """
  @spec egg_count(number :: non_neg_integer) :: non_neg_integer
  def egg_count(number), do: shift_count(number, 0)

  @spec shift_count(non_neg_integer, non_neg_integer) :: non_neg_integer
  defp shift_count(0, sum), do: sum
  defp shift_count(n, sum), do: shift_count(n >>> 1, sum + (n &&& 1))
end
