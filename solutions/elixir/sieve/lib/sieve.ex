defmodule Sieve do
  @doc """
  Generates a list of primes up to a given limit.
  """
  @spec primes_to(non_neg_integer) :: [non_neg_integer]
  def primes_to(limit) when limit < 2, do: []

  def primes_to(limit) do
    Enum.to_list(2..limit)
    |> find_primes(limit, [])
    |> Enum.reverse()
  end

  @spec find_primes([integer], integer, [integer]) :: [integer]
  defp find_primes([], _, primes), do: primes

  defp find_primes([prime | rest], limit, primes) do
    (rest -- Enum.to_list((prime * prime)..limit//prime))
    |> find_primes(limit, [prime | primes])
  end
end
