defmodule WordCount do
  @doc """
  Count the number of words in the sentence.

  Words are compared case-insensitively.
  """
  @spec count(String.t()) :: map
  def count(sentence) do
    sentence
    |> String.downcase()
    |> String.replace(~r/[^\w-']|_/u, "\s")
    |> String.split()
    |> Enum.map(&String.trim(&1, ~s/'/))
    |> Enum.frequencies()
  end
end
