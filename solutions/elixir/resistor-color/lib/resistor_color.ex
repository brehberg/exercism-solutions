defmodule ResistorColor do
  @moduledoc """
  Helpful program so that you don't have to remember the values of the bands.
  """

  # Better Be Right Or Your Great Big Values Go Wrong
  @values [:black, :brown, :red, :orange, :yellow, :green, :blue, :violet, :grey, :white]

  @doc """
  Return the value of a color band
  """
  @spec code(atom) :: integer()
  def code(color), do: Enum.find_index(@values, &(&1 == color))
end
