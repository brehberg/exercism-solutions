defmodule Sublist do
  @moduledoc false
  import Enum, only: [take: 2]

  @doc """
  Returns whether the first list is a sublist or a superlist of the second list
  and if not whether it is equal or unequal to the second list.
  """
  @spec compare([], []) :: :sublist | :superlist | :equal | :unequal
  def compare(a, a), do: :equal

  def compare(a, b) do
    cond do
      length(a) < length(b) and sublist?(a, b) -> :sublist
      length(a) > length(b) and sublist?(b, a) -> :superlist
      true -> :unequal
    end
  end

  @doc false
  @spec sublist?([], []) :: boolean
  defp sublist?(a, b) when length(a) > length(b), do: false

  defp sublist?(a, b) do
    case compare(a, b |> take(length(a))) do
      :equal -> true
      :unequal -> sublist?(a, tl(b))
    end
  end
end
