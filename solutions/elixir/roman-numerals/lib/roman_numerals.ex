defmodule RomanNumerals do
  @moduledoc false
  import Enum, only: [reverse: 1]
  @values [{1, ~w(I V X)}, {10, ~w(X L C)}, {100, ~w(C D M)}]

  @doc """
  Convert the number to a roman number.
  """
  @spec numeral(pos_integer) :: String.t()
  def numeral(number),
    do: build(@values) |> reverse() |> cypher(number, "")

  @doc false
  @spec build([{integer, [String.t()]}]) :: [{integer, String.t()}]
  defp build([]), do: []

  defp build([{value, [one, five, ten]} | rest]) do
    [
      {value, one},
      {value * 4, one <> five},
      {value * 5, five},
      {value * 9, one <> ten},
      {value * 10, ten} | build(rest)
    ]
  end

  @doc false
  @spec cypher([], non_neg_integer, String.t()) :: String.t()
  defp cypher(_, 0, result), do: result

  defp cypher([{value, glyph} | _] = pairs, n, str) when n >= value,
    do: cypher(pairs, n - value, str <> glyph)

  defp cypher([_ | rest], n, str), do: cypher(rest, n, str)
end
