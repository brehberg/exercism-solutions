defmodule PrimeFactors do
  @moduledoc false

  @doc """
  Compute the prime factors for 'number'.

  The prime factors are prime numbers that when multiplied give the desired
  number.

  The prime factors of 'number' will be ordered lowest to highest.
  """
  @spec factors_for(pos_integer) :: [pos_integer]
  def factors_for(number), do: find_factors(number, 2, [])

  @doc false
  @spec find_factors(pos_integer, pos_integer, []) :: [pos_integer]
  defp find_factors(1, _, list), do: Enum.sort(list)

  defp find_factors(n, next, list) when rem(n, next) == 0,
    do: find_factors(div(n, next), next, [next | list])

  defp find_factors(n, next, list),
    do: find_factors(n, next + 1, list)
end
