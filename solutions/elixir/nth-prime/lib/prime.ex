defmodule Prime do
  @moduledoc false
  import Integer, only: [pow: 2]
  @starting_primes [2, 3, 5, 7, 11]

  @doc """
  Generates the nth prime.
  """
  @spec nth(non_neg_integer) :: non_neg_integer
  def nth(count) when count >= 6 do
    # prime(n) < n*(log n + log (log n)), for n >= 6
    limit = floor(count * (:math.log(count) + :math.log(:math.log(count))))

    # initial candidate list is all odd numbers starting from 3
    Enum.to_list(3..limit//2)
    |> sieve(limit, [2])
    |> Enum.at(count - 1)
  end
  def nth(count) when count >= 1,
    do: @starting_primes |> Enum.at(count - 1)

  # Sieve of Eratosthenes algorithm for finding all prime numbers up to the given limit.
  @spec sieve([integer], integer, [integer]) :: [integer]
  defp sieve([final], _, primes), do: [final | primes] |> Enum.reverse()

  defp sieve([next | rest], limit, primes) do
    # remove all odd multiples of next prime starting from that prime squared
    (rest -- Enum.to_list(pow(next, 2)..limit//2 * next))
    |> sieve(limit, [next | primes])
  end
end
