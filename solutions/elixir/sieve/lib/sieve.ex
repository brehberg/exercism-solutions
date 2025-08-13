defmodule Sieve do
  @doc """
  Generates a list of primes up to a given limit.
  """
  @spec primes_to(non_neg_integer) :: [non_neg_integer]
  def primes_to(limit) when limit < 2, do: []

  def primes_to(limit) do
    ([true, true] ++ List.duplicate(false, limit - 1))
    |> find_primes(limit, [])
    |> Enum.reverse()
  end

  @spec find_primes([boolean], non_neg_integer, [non_neg_integer]) :: [non_neg_integer]
  defp find_primes(marked, limit, primes) do
    case Enum.find_index(marked, &(!&1)) do
      nil ->
        primes

      prime ->
        prime..limit//prime
        |> Enum.reduce(marked, &List.replace_at(&2, &1, true))
        |> find_primes(limit, [prime | primes])
    end
  end
end
