defmodule Allergies do
  @moduledoc """
  Given a person's allergy score, determine whether or not they're allergic
  to a given item, and their full list of allergies.
  """

  # The list of items (and their value) that were tested are:
  @allergens %{
    "eggs" => 1,
    "peanuts" => 2,
    "shellfish" => 4,
    "strawberries" => 8,
    "tomatoes" => 16,
    "chocolate" => 32,
    "pollen" => 64,
    "cats" => 128
  }

  @doc """
  List the allergies for which the corresponding flag bit is true.
  """
  @spec list(non_neg_integer) :: [String.t()]
  def list(flags),
    do: for({key, val} <- @allergens, flags |> contains?(val), do: key)

  @doc """
  Returns whether the corresponding flag bit in 'flags' is set for the item.
  """
  @spec allergic_to?(non_neg_integer, String.t()) :: boolean
  def allergic_to?(flags, item), do: flags |> contains?(@allergens[item])

  @spec contains?(non_neg_integer, non_neg_integer) :: boolean
  defp contains?(flags, value), do: Bitwise.band(flags, value) == value
end
