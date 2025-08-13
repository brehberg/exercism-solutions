defmodule Anagram do
  @moduledoc false
  import String, only: [upcase: 1]

  @doc """
  Returns all candidates that are anagrams of, but not equal to, 'base'.
  """
  @spec match(String.t(), [String.t()]) :: [String.t()]
  def match(base, candidates),
    do: candidates |> Enum.filter(&anagram?(upcase(&1), upcase(base)))

  @doc false
  @spec anagram?(String.t(), String.t()) :: boolean
  defp anagram?(base, base), do: false
  defp anagram?(word, base), do: sorted(word) == sorted(base)

  @doc false
  @spec sorted(String.t()) :: charlist()
  defp sorted(input), do: input |> to_charlist() |> Enum.sort()
end
