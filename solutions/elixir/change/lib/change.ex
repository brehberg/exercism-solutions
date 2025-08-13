defmodule Change do
  @type coins_by_value :: %{integer => [integer]}

  @doc """
    Determine the least number of coins to be given to the user such
    that the sum of the coins' value would equal the correct amount of change.
    It returns {:error, "cannot change"} if it is not possible to compute the
    right amount of coins. Otherwise returns the tuple {:ok, list_of_coins}

    ## Examples

      iex> Change.generate([5, 10, 15], 3)
      {:error, "cannot change"}

      iex> Change.generate([1, 5, 10], 18)
      {:ok, [1, 1, 1, 5, 10]}

  """
  @spec generate(list, integer) :: {:ok, list} | {:error, String.t()}
  def generate(_, target) when target < 0, do: {:error, "cannot change"}

  def generate(coins, target) do
    case do_make_change(Enum.reverse(coins), target) do
      [nil] -> {:error, "cannot change"}
      change -> {:ok, change}
    end
  end

  @spec do_make_change([integer], integer) :: [integer]
  defp do_make_change(coins, target) do
    %{0 => []}
    |> find_all_combos(coins, target, 1)
    |> Map.fetch!(target)
    |> Enum.sort()
  end

  @spec find_all_combos(coins_by_value, [integer], integer, integer) :: coins_by_value
  defp find_all_combos(combos, _, max, n) when n > max, do: combos

  defp find_all_combos(combos, coins, max, n) do
    Map.put(combos, n, find_min_coins(combos, coins, n))
    |> find_all_combos(coins, max, n + 1)
  end

  @spec find_min_coins(coins_by_value, [integer], integer) :: [integer]
  defp find_min_coins(combos, coins, n) do
    for coin when coin <= n <- coins do
      [coin | combos[n - coin]]
    end
    |> Enum.reject(&(nil in &1))
    |> Enum.sort_by(&length/1)
    |> List.first([nil])
  end
end
