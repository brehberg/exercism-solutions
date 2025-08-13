defmodule Anagram do
  @moduledoc false

  @doc """
  Returns all candidates that are anagrams of, but not equal to, 'base'.
  """
  @spec match(String.t(), [String.t()]) :: [String.t()]
  def match(base, candidates) do
    candidates
    |> Enum.reject(&String.match?(&1, Regex.compile!(base, "i")))
    |> Enum.map(&check_anagram(&1, transform(base)))
    |> Enum.filter(& &1)
  end

  @doc false
  @spec check_anagram(String.t(), String.t()) :: String.t()
  defp check_anagram(candidate, sorted),
    do: if(transform(candidate) == sorted, do: candidate, else: nil)

  @doc false
  @spec transform(String.t()) :: String.t()
  defp transform(input),
    do: input |> String.upcase() |> String.split("") |> Enum.sort() |> Enum.join()
end
