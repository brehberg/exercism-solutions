defmodule BoutiqueInventory do
  @moduledoc "Functions for running an online fashion boutique."
  @type item :: %{name: String.t(), price: integer, quantity_by_size: %{atom => integer}}

  @doc "Sort items by price"
  @spec sort_by_price([item]) :: [item]
  def sort_by_price(inventory), do: inventory |> Enum.sort_by(& &1.price)

  @doc "Find all items with missing prices"
  @spec with_missing_price([item]) :: [item]
  def with_missing_price(inventory), do: inventory |> Enum.reject(& &1.price)

  @doc "Update item names"
  @spec update_names([item], String.t(), String.t()) :: [item]
  def update_names(inventory, old_word, new_word),
    do: inventory |> Enum.map(&replace_item_name(&1, old_word, new_word))

  @doc false
  @spec replace_item_name(item, old :: String.t(), new :: String.t()) :: item
  defp replace_item_name(%{name: name} = item, old, new),
    do: %{item | name: String.replace(name, old, new)}

  @doc "Increment the item's quantity"
  @spec increase_quantity(item, integer) :: item
  def increase_quantity(%{quantity_by_size: qtys} = item, n),
    do: %{item | quantity_by_size: qtys |> Map.new(fn {id, qty} -> {id, qty + n} end)}

  @doc "Calculate the item's total quantity"
  @spec total_quantity(item) :: integer
  def total_quantity(%{quantity_by_size: qtys}),
    do: qtys |> Enum.reduce(0, fn {_, qty}, sum -> qty + sum end)
end
