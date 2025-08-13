defmodule Say do
  @moduledoc false
  @ones ~w(zero one two three four five six seven eight nine)
  @teens ~w(ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen)
  @tens ~w(zero ten twenty thirty forty fifty sixty seventy eighty ninety)
  @groups ["", " thousand", " million", " billion"]
  @tens_sep "-"
  @hundred_sep " hundred "

  @doc """
  Translate a positive integer into English.
  """
  @spec in_english(integer) :: {atom, String.t()}
  def in_english(number) when number < 0 or number >= 1_000_000_000_000,
    do: {:error, "number is out of range"}

  def in_english(0), do: {:ok, Enum.at(@ones, 0)}

  def in_english(number) do
    {:ok,
     number
     |> to_string()
     |> String.reverse()
     |> String.graphemes()
     |> Enum.map(&String.to_integer/1)
     |> Enum.chunk_every(3, 3, [0, 0])
     |> Enum.zip(@groups)
     |> Enum.reverse()
     |> Enum.map(&{Enum.reverse(elem(&1, 0)), elem(&1, 1)})
     |> Enum.map_join("\s", &parse_group/1)
     |> String.trim()}
  end

  defp parse_group({[0, 0, 0], _}), do: ""
  defp parse_group({[0, 0, one], label}), do: Enum.at(@ones, one) <> label
  defp parse_group({[0, ten, 0], label}), do: Enum.at(@tens, ten) <> label

  defp parse_group({[0, ten, one], label}) when ten < 2,
    do: Enum.at(@teens, one) <> label

  defp parse_group({[0, ten, one], label}),
    do: Enum.at(@tens, ten) <> @tens_sep <> Enum.at(@ones, one) <> label

  defp parse_group({[hun, 0, 0], label}),
    do: Enum.at(@ones, hun) <> String.trim_trailing(@hundred_sep) <> label

  defp parse_group({[hun, ten, one], label}),
    do: Enum.at(@ones, hun) <> @hundred_sep <> parse_group({[0, ten, one], label})
end
