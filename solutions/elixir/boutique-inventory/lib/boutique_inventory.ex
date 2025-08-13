defmodule BoutiqueInventory do
  @moduledoc """
  Functions for running an online fashion boutique.
  """
  @type item :: %{
          name: String.t(),
          price: integer,
          quantity_by_size: %{s: integer, m: integer, l: integer, xl: integer}
        }

  @doc """
  Sort items by price
  """
  @spec sort_by_price(list(item)) :: list(item)
  def sort_by_price(inventory), do: inventory |> Enum.sort_by(& &1.price)

  @doc """
  Find all items with missing prices
  """
  @spec with_missing_price(list(item)) :: list(item)
  def with_missing_price(inventory), do: inventory |> Enum.reject(& &1.price)

  @doc """
  Update item names
  """
  @spec update_names(list(item), String.t(), String.t()) :: list(item)
  def update_names(inventory, old_word, new_word),
    do: inventory |> Enum.map(&replace_item_name(&1, old_word, new_word))

  @spec replace_item_name(item, String.t(), String.t()) :: item
  defp replace_item_name(item, old_word, new_word),
    do: Map.replace(item, :name, String.replace(item.name, old_word, new_word))

  @doc """
  Increment the item's quantity
  """
  @spec increase_quantity(item, integer) :: item
  def increase_quantity(item, count),
    do:
      Map.replace(
        item,
        :quantity_by_size,
        item.quantity_by_size |> Map.new(fn {type, qty} -> {type, qty + count} end)
      )

  @doc """
  Calculate the item's total quantity
  """
  @spec total_quantity(item) :: integer
  def total_quantity(item),
    do: item.quantity_by_size |> Enum.reduce(0, fn {_, qty}, sum -> qty + sum end)
end
