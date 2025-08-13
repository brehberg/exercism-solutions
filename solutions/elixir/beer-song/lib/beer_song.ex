defmodule BeerSong do
  @moduledoc false

  @doc """
  Get a single verse of the beer song
  """
  @spec verse(integer) :: String.t()
  def verse(0) do
    """
    No more bottles of beer on the wall, no more bottles of beer.
    Go to the store and buy some more, 99 bottles of beer on the wall.
    """
  end

  def verse(1), do: verse_template("1 bottle", "no more bottles", "it")
  def verse(2), do: verse_template("2 bottles", "1 bottle")
  def verse(number), do: verse_template("#{number} bottles", "#{number - 1} bottles")

  @doc false
  @spec verse_template(String.t(), String.t(), String.t()) :: String.t()
  defp verse_template(count1, count2, one \\ "one") do
    """
    #{count1} of beer on the wall, #{count1} of beer.
    Take #{one} down and pass it around, #{count2} of beer on the wall.
    """
  end

  @doc """
  Get the entire beer song for a given range of numbers of bottles.
  """
  @spec lyrics(Range.t()) :: String.t()
  def lyrics(range \\ 99..0), do: Enum.map_join(range, "\n", &verse/1)
end
