defmodule TwelveDays do
  @days ~w(
    first second third fourth fifth sixth
    seventh eighth ninth tenth eleventh twelfth
  )

  @gifts [
    ~s(twelve Drummers Drumming,),
    ~s(eleven Pipers Piping,),
    ~s(ten Lords-a-Leaping,),
    ~s(nine Ladies Dancing,),
    ~s(eight Maids-a-Milking,),
    ~s(seven Swans-a-Swimming,),
    ~s(six Geese-a-Laying,),
    ~s(five Gold Rings,),
    ~s(four Calling Birds,),
    ~s(three French Hens,),
    ~s(two Turtle Doves, and),
    ~s(a Partridge in a Pear Tree.)
  ]

  @doc """
  Given a `number`, return the song's verse for that specific day, including
  all gifts for previous days in the same line.
  """
  @spec verse(number :: integer) :: String.t()
  def verse(number),
    do: [opener(number) | Enum.drop(@gifts, 12 - number)] |> Enum.join(" ")

  @spec opener(integer) :: String.t()
  defp opener(n),
    do: "On the #{Enum.at(@days, n - 1)} day of Christmas my true love gave to me:"

  @doc """
  Given a `starting_verse` and an `ending_verse`, return the verses for each
  included day, one per line.
  """
  @spec verses(starting_verse :: integer, ending_verse :: integer) :: String.t()
  def verses(starting_verse, ending_verse),
    do: starting_verse..ending_verse |> Enum.map_join("\n", &verse/1)

  @doc """
  Sing all 12 verses, in order, one verse per line.
  """
  @spec sing() :: String.t()
  def sing, do: verses(1, 12)
end
