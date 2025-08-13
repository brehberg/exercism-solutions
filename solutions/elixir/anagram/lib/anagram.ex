defmodule Anagram do
  @moduledoc false

  @doc """
  Returns all candidates that are anagrams of, but not equal to, 'base'.
  """
  @spec match(String.t(), [String.t()]) :: [String.t()]
  def match(base, candidates) do
    candidates
    |> Enum.reject(&String.match?(&1, Regex.compile!(base, "i")))
    |> Enum.filter(&check_anagram?(&1, transform(base)))
  end

  @doc false
  @spec check_anagram(String.t(), String.t()) :: boolean
  defp check_anagram(candidate, sorted),
    do: transform(candidate) == sorted

  @doc false
  @spec transform(String.t()) :: [String.t()]
  defp transform(input),
    do: input |> String.upcase() |> String.split("") |> Enum.sort()
end
