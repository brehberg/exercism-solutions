defmodule KitchenCalculator do
  def get_volume({_, volume}), do: volume

  def to_milliliter({:cup, volume}), do: {:milliliter, volume * 240}
  def to_milliliter({:fluid_ounce, volume}), do: {:milliliter, volume * 30}
  def to_milliliter({:teaspoon, volume}), do: {:milliliter, volume * 5}
  def to_milliliter({:tablespoon, volume}), do: {:milliliter, volume * 15}
  def to_milliliter({:milliliter, volume}), do: {:milliliter, volume}

  def from_milliliter({:milliliter, volume}, :cup = unit), do: {unit, volume / 240}
  def from_milliliter({:milliliter, volume}, :fluid_ounce = unit), do: {unit, volume / 30}
  def from_milliliter({:milliliter, volume}, :teaspoon = unit), do: {unit, volume / 5}
  def from_milliliter({:milliliter, volume}, :tablespoon = unit), do: {unit, volume / 15}
  def from_milliliter({:milliliter, volume}, :milliliter = unit), do: {unit, volume}

  def convert(volume_pair, unit), do: volume_pair |> to_milliliter() |> from_milliliter(unit)
end
