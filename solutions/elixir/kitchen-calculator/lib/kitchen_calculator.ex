defmodule KitchenCalculator do
  def get_volume({_, volume}), do: volume

  def to_milliliter({:cup, volume}), do: {:milliliter, volume * 240}
  def to_milliliter({:fluid_ounce, volume}), do: {:milliliter, volume * 30}
  def to_milliliter({:teaspoon, volume}), do: {:milliliter, volume * 5}
  def to_milliliter({:tablespoon, volume}), do: {:milliliter, volume * 15}
  def to_milliliter({:milliliter, volume}), do: {:milliliter, volume}

  def from_milliliter(volume_pair, :cup = unit), do: {unit, get_volume(volume_pair) / 240}
  def from_milliliter(volume_pair, :fluid_ounce = unit), do: {unit, get_volume(volume_pair) / 30}
  def from_milliliter(volume_pair, :teaspoon = unit), do: {unit, get_volume(volume_pair) / 5}
  def from_milliliter(volume_pair, :tablespoon = unit), do: {unit, get_volume(volume_pair) / 15}
  def from_milliliter(volume_pair, :milliliter = unit), do: {unit, get_volume(volume_pair)}

  def convert(volume_pair, unit), do: volume_pair |> to_milliliter() |> from_milliliter(unit)
end
