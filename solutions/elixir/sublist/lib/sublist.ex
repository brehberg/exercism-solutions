defmodule Sublist do
  @moduledoc false
  import Enum, only: [take: 2]

  @doc """
  Returns whether the first list is a sublist or a superlist of the second list
  and if not whether it is equal or unequal to the second list.
  """
  def compare(a, b) when length(a) < length(b),
    do: if(sublist?(a, b), do: :sublist, else: :unequal)

  def compare(a, b) when length(a) > length(b),
    do: if(sublist?(b, a), do: :superlist, else: :unequal)

  def compare(a, a), do: :equal
  def compare(_, _), do: :unequal

  defp sublist?(a, b) when length(a) > length(b), do: false

  defp sublist?(a, b) do
    case compare(a, b |> take(length(a))) do
      :equal -> true
      :unequal -> sublist?(a, tl(b))
    end
  end
end
