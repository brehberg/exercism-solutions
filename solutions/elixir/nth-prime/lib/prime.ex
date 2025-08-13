defmodule Prime do
  @moduledoc false
  import Integer, only: [pow: 2]

  @doc """
  Generates the nth prime.
  """
  @spec nth(non_neg_integer) :: non_neg_integer
  def nth(1), do: 2
  def nth(2), do: 3
  def nth(3), do: 5
  def nth(4), do: 7
  def nth(5), do: 11

  def nth(count) when count >= 6 do
    # p(n) < n*(log n + log (log n)), for n >= 6
    limit = floor(count * (:math.log(count) + :math.log(:math.log(count))))

    Enum.to_list(3..limit//2)
    |> sieve(limit, [2])
    |> Enum.at(count - 1)
  end

  # Sieve of Eratosthenes algorithm for finding all prime numbers up to the given limit.
  @spec sieve([integer], integer, [integer]) :: [integer]
  defp sieve([prime], _, primes), do: [prime | primes] |> Enum.reverse()

  defp sieve([prime | rest], limit, primes) do
    (rest -- Enum.to_list(pow(prime, 2)..limit//2 * prime))
    |> sieve(limit, [prime | primes])
  end
end
