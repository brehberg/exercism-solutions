defmodule PrimeFactors do
  @moduledoc false

  @doc """
  Compute the prime factors for 'number'.

  The prime factors are prime numbers that when multiplied give the desired
  number.

  The prime factors of 'number' will be ordered lowest to highest.
  """
  @spec factors_for(pos_integer) :: [pos_integer]
  def factors_for(1), do: []
  def factors_for(number), do: find_factors(number, 2, [])

  @doc false
  @spec find_factors(integer, integer, []) :: [pos_integer]
  defp find_factors(n, next, primes) when div(n, next) < next,
    do: [n | primes] |> Enum.reverse()

  defp find_factors(n, next, primes) when rem(n, next) == 0,
    do: find_factors(div(n, next), next, [next | primes])

  defp find_factors(n, next, primes),
    do: find_factors(n, next + 1, primes)
end
