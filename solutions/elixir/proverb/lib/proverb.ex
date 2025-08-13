defmodule Proverb do
  @doc """
  Generate a proverb from a list of strings.
  """
  @spec recite(strings :: [String.t()]) :: String.t()
  def recite([]), do: ""

  def recite(strings) do
    strings
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.concat([Enum.take(strings, 1)])
    |> Enum.map_join(&prepare_verse/1)
  end

  @doc false
  @spec prepare_verse([String.t()]) :: String.t()
  defp prepare_verse([first, second]),
    do: "For want of a #{first} the #{second} was lost.\n"

  defp prepare_verse([single]),
    do: "And all for the want of a #{single}.\n"
end
