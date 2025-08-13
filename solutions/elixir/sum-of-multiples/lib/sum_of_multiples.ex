defmodule SumOfMultiples do
  @doc """
  Adds up all numbers from 1 to a given end number that are multiples
  of the factors provided.
  """
  @spec to(non_neg_integer, [non_neg_integer]) :: non_neg_integer
  def to(limit, factors) do
    factors
    |> Enum.reject(&(&1 == 0))
    |> Enum.map(&Range.new(&1, limit - 1, &1))
    |> Enum.map(&Enum.to_list/1)
    |> List.flatten()
    |> Enum.uniq()
    |> Enum.sum()
  end
end
