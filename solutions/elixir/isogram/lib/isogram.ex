defmodule Isogram do
  @moduledoc false
  @allowed ~r/[-\s]/

  @doc """
  Determines if a word or sentence is an isogram
  """
  @spec isogram?(String.t()) :: boolean
  def isogram?(sentence) do
    sentence
    |> String.replace(@allowed, "")
    |> String.downcase()
    |> String.graphemes()
    |> Enum.frequencies()
    |> Map.reject(&(elem(&1, 1) == 1))
    |> Enum.empty?()
  end
end
