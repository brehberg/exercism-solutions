defmodule Series do
  @doc """
  Finds the largest product of a given number of consecutive numbers in a given string of numbers.
  """
  @spec largest_product(String.t(), non_neg_integer) :: non_neg_integer
  def largest_product(_, 0), do: 1
  def largest_product(_, size) when size < 0, do: raise(ArgumentError)
  def largest_product(s, size) when size > byte_size(s), do: raise(ArgumentError)

  def largest_product(number_string, size) do
    number_string
    |> String.to_integer()
    |> Integer.digits()
    |> Enum.chunk_every(size, 1, :discard)
    |> Enum.map(&Enum.product/1)
    |> Enum.max(&>=/2, fn -> 0 end)
  end
end
