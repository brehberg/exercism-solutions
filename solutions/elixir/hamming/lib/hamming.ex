defmodule Hamming do
  @doc """
  Returns number of differences between two strands of DNA, known as the Hamming Distance.

  ## Examples

  iex> Hamming.hamming_distance('AAGTCATA', 'TAGCGATC')
  {:ok, 4}
  """
  @spec hamming_distance([char], [char]) :: {:ok, non_neg_integer} | {:error, String.t()}
  def hamming_distance(strand1, strand2) when length(strand1) == length(strand2),
    do: do_hamming(strand1, strand2, 0)

  def hamming_distance(_, _),
    do: {:error, "strands must be of equal length"}

  @doc false
  defp do_hamming([], [], count), do: {:ok, count}

  defp do_hamming([c1 | rest1], [c2 | rest2], count) when c1 == c2,
    do: do_hamming(rest1, rest2, count)

  defp do_hamming([_ | rest1], [_ | rest2], count),
    do: do_hamming(rest1, rest2, count + 1)
end
