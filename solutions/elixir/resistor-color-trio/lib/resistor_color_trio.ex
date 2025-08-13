defmodule ResistorColorTrio do
  @moduledoc """
  Helpful program so that you don't have to remember the values of resistor bands.
  """

  # Better Be Right Or Your Great Big Values Go Wrong
  @values [:black, :brown, :red, :orange, :yellow, :green, :blue, :violet, :grey, :white]

  @doc """
  Calculate the resistance value in ohms from resistor colors
  """
  @spec label(colors :: [atom]) :: {number, :ohms | :kiloohms | :megaohms | :gigaohms}
  def label(colors), do: value(colors) |> format_label()

  @doc false
  @spec label(number) :: {number, :ohms | :kiloohms | :megaohms | :gigaohms}
  defp format_label(r) when r < 1_000, do: {r, :ohms}
  defp format_label(r) when r < 1_000_000, do: {r / 1_000, :kiloohms}
  defp format_label(r) when r < 1_000_000_000, do: {r / 1_000_000, :megaohms}
  defp format_label(r), do: {r / 1_000_000_000, :gigaohms}

  @doc false
  @spec value(colors :: [atom]) :: number
  defp value([tens, ones, exponent | _]),
    do: (value(tens) * 10 + value(ones)) * Integer.pow(10, value(exponent))

  defp value(color), do: Enum.find_index(@values, &(&1 == color))
end
