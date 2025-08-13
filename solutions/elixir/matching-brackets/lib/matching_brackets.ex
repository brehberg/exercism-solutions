defmodule MatchingBrackets do
  @moduledoc false
  @open ~w([ { \()
  @close ~w(] } \))
  @match %{"[" => "]", "{" => "}", "(" => ")"}

  @doc """
  Checks that all the brackets and braces in the string are matched correctly, and nested correctly
  """
  @spec check_brackets(String.t()) :: boolean
  def check_brackets(str), do: closers_match?(str, [])

  @doc false
  @spec closers_match?(String.t(), [String.t()]) :: boolean
  defp closers_match?("", []), do: true
  defp closers_match?("", _), do: false

  defp closers_match?(<<first::binary-size(1), rest::binary>>, closers) do
    cond do
      first in @close and closers == [] -> false
      first in @close and hd(closers) != first -> false
      first in @close -> closers_match?(rest, tl(closers))
      first in @open -> closers_match?(rest, [@match[first] | closers])
      true -> closers_match?(rest, closers)
    end
  end
end
