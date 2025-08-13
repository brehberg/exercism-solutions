defmodule KitchenCalculator do
  def get_volume({_, volume}), do: volume

  def to_milliliter({:cup,_} = volume_pair), do: {:milliliter, get_volume(volume_pair) * 240}
  def to_milliliter({:fluid_ounce,_} = volume_pair), do: {:milliliter, get_volume(volume_pair) * 30}
  def to_milliliter({:teaspoon,_} = volume_pair), do: {:milliliter, get_volume(volume_pair) * 5}
  def to_milliliter({:tablespoon,_} = volume_pair), do: {:milliliter, get_volume(volume_pair) * 15}
  def to_milliliter({:milliliter,_} = volume_pair), do: {:milliliter, get_volume(volume_pair)}

  def from_milliliter(volume_pair, :cup = unit), do: {unit, get_volume(volume_pair) / 240}
  def from_milliliter(volume_pair, :fluid_ounce = unit), do: {unit, get_volume(volume_pair) / 30}
  def from_milliliter(volume_pair, :teaspoon = unit), do: {unit, get_volume(volume_pair) / 5}
  def from_milliliter(volume_pair, :tablespoon = unit), do: {unit, get_volume(volume_pair) / 15}
  def from_milliliter(volume_pair, :milliliter = unit), do: {unit, get_volume(volume_pair)}

  def convert(volume_pair, unit), do: from_milliliter(to_milliliter(volume_pair), unit)
end
