defmodule ResistorColorTrio do
  @moduledoc """
  Helpful program so that you don't have to remember the values of resistor bands.
  """

  # Better Be Right Or Your Great Big Values Go Wrong
  @values ~w[black brown red orange yellow green blue violet grey white]a

  @doc """
  Calculate the resistance value in ohms from resistor colors
  """
  @spec label(colors :: [atom]) :: {number, :ohms | :kiloohms | :megaohms | :gigaohms}
  def label(colors), do: value(colors) |> format(:ohms)

  @doc false
  @spec label(number, atom) :: {number, :ohms | :kiloohms | :megaohms | :gigaohms}
  defp format(r, :ohms) when r >= 1000, do: format(r / 1000, :kiloohms)
  defp format(r, :kiloohms) when r >= 1000, do: format(r / 1000, :megaohms)
  defp format(r, :megaohms) when r >= 1000, do: format(r / 1000, :gigaohms)
  defp format(r, unit), do: {r, unit}

  @doc false
  @spec value(colors :: [atom]) :: number
  defp value([tens, ones, exponent | _]),
    do: (value(tens) * 10 + value(ones)) * Integer.pow(10, value(exponent))

  defp value(color), do: Enum.find_index(@values, &(&1 == color))
end
