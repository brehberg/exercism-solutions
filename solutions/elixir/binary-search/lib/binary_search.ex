defmodule BinarySearch do
  @moduledoc false
  @typep search_range :: {low :: integer, high :: integer}
  @typep search_result :: {:ok, non_neg_integer} | :not_found

  @doc """
    Searches for a key in the tuple using the binary search algorithm.
    It returns :not_found if the key is not in the tuple.
    Otherwise returns {:ok, index}.

    ## Examples

      iex> BinarySearch.search({}, 2)
      :not_found

      iex> BinarySearch.search({1, 3, 5}, 2)
      :not_found

      iex> BinarySearch.search({1, 3, 5}, 5)
      {:ok, 2}

  """

  @spec search(tuple, integer) :: {:ok, integer} | :not_found
  def search(numbers, key),
    do: binary_search(numbers, key, {0, tuple_size(numbers) - 1})

  @doc false
  @spec binary_search(tuple, integer, search_range) :: search_result
  defp binary_search(_, _, {low, high}) when low > high, do: :not_found

  defp binary_search(numbers, key, {low, high}) do
    mid = (low + high) |> div(2)
    n = numbers |> elem(mid)

    cond do
      n > key -> binary_search(numbers, key, {low, mid - 1})
      n < key -> binary_search(numbers, key, {mid + 1, high})
      true -> {:ok, mid}
    end
  end
end
