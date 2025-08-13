defmodule Sublist do
  @moduledoc false
  import Enum, only: [take: 2]

  @doc """
  Returns whether the first list is a sublist or a superlist of the second list
  and if not whether it is equal or unequal to the second list.
  """
  def compare(a, b) when length(a) < length(b), do: consider_sublist(a, b)
  def compare(a, b) when length(a) > length(b), do: consider_superlist(a, b)
  def compare(a, b), do: do_compare(a, b)

  defp do_compare([], []), do: :equal
  defp do_compare([head | a], [head | b]), do: do_compare(a, b)
  defp do_compare(_, _), do: :unequal

  defp consider_sublist(a, b) when length(a) > length(b), do: :unequal

  defp consider_sublist(a, b) do
    case do_compare(a, b |> take(length(a))) do
      :equal -> :sublist
      :unequal -> consider_sublist(a, tl(b))
    end
  end

  defp consider_superlist(a, b) when length(a) < length(b), do: :unequal

  defp consider_superlist(a, b) do
    case do_compare(a |> take(length(b)), b) do
      :equal -> :superlist
      :unequal -> consider_superlist(tl(a), b)
    end
  end
end
