defmodule ResistorColorDuo do
  @values [:black, :brown, :red, :orange, :yellow, :green, :blue, :violet, :grey, :white]

  @doc """
  Calculate a resistance value from two colors
  """
  @spec value(colors :: [atom]) :: integer
  def value([tens | [ones | _]]),
    do: Enum.find_index(@values, &(&1 == tens)) * 10 + Enum.find_index(@values, &(&1 == ones))

  def value([single | _]), do: Enum.find_index(@values, &(&1 == single))
  def value([]), do: 0
end
