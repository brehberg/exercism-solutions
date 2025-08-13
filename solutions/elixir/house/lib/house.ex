defmodule House do
  @moduledoc false
  @prefix ~s(This is)
  @parts [
    ~s(the horse and the hound and the horn that belonged to),
    ~s(the farmer sowing his corn that kept),
    ~s(the rooster that crowed in the morn that woke),
    ~s(the priest all shaven and shorn that married),
    ~s(the man all tattered and torn that kissed),
    ~s(the maiden all forlorn that milked),
    ~s(the cow with the crumpled horn that tossed),
    ~s(the dog that worried),
    ~s(the cat that killed),
    ~s(the rat that ate),
    ~s(the malt that lay in),
    ~s(the house that Jack built.\n)
  ]

  @doc """
  Return verses of the nursery rhyme 'This is the House that Jack Built'.
  """
  @spec recite(start :: integer, stop :: integer) :: String.t()
  def recite(start, stop) do
    start..stop
    |> Enum.map(&prepare_verse/1)
    |> Enum.map_join(&Enum.join(&1, "\s"))
  end

  @doc false
  @spec prepare_verse(integer) :: [String.t()]
  defp prepare_verse(n), do: [@prefix | Enum.drop(@parts, length(@parts) - n)]
end
