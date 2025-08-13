defmodule RomanNumerals do
  @moduledoc false
  @values [~w(M D C), ~w(C L X), ~w(X V I)]

  @doc """
  Convert the number to a roman number.
  """
  @spec numeral(pos_integer) :: String.t()
  def numeral(number),
    do: build(100, @values) |> cypher(number, "")

  @doc false
  @spec build(integer, [[String.t()]]) :: [{integer, String.t()}]
  defp build(_, []), do: []

  defp build(value, [[ten, five, one] | rest]) do
    [
      {value * 10, ten},
      {value * 9, one <> ten},
      {value * 5, five},
      {value * 4, one <> five},
      {value, one}
    ] ++ build(div(value, 10), rest)
  end

  @doc false
  @spec cypher([], non_neg_integer, String.t()) :: String.t()
  defp cypher(_, 0, result), do: result

  defp cypher([{value, glyph} | _] = pairs, n, str) when n >= value,
    do: cypher(pairs, n - value, str <> glyph)

  defp cypher([_ | rest], n, str), do: cypher(rest, n, str)
end
