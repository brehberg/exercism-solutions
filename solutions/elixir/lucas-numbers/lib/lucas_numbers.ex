defmodule LucasNumbers do
  @moduledoc """
  Lucas numbers are an infinite sequence of numbers which build progressively
  which hold a strong correlation to the golden ratio (φ or ϕ)

  E.g.: 2, 1, 3, 4, 7, 11, 18, 29, ...
  """
  @spec generate(pos_integer) :: [integer]
  def generate(count) when not is_integer(count) or count < 1,
    do: raise(ArgumentError, "count must be specified as an integer >= 1")

  def generate(1), do: [2]
  def generate(2), do: [2, 1]
  def generate(count), do: [2 | generate_rest(count - 1)]

  defp generate_rest(count) do
    Stream.iterate({2, 1}, &{elem(&1, 1), Tuple.sum(&1)})
    |> Enum.take(count)
    |> Enum.map(&elem(&1, 1))
  end
end
