defmodule KitchenCalculator do
  @moduledoc """
  Convert common US baking measurements to milliliters (mL)

    Unit to convert   volume  milliliters (mL)
      milliliter        1       1
      US cup            1       240
      US fluid ounce    1       30
      US teaspoon       1       5
      US tablespoon     1       15
  """
  @type volume_unit :: :cup | :fluid_ounce | :teaspoon | :tablespoon | :milliliter
  @type volume_pair :: {volume_unit(), number()}

  @doc "Get the numeric component from a volume-pair"
  @spec get_volume(volume_pair()) :: number
  def get_volume({_, volume}), do: volume

  @doc "Convert the volume-pair to milliliters"
  @spec to_milliliter(volume_pair()) :: {:milliliter, number}
  def to_milliliter({unit, volume}) do
    case unit do
      :cup -> {:milliliter, volume * 240}
      :fluid_ounce -> {:milliliter, volume * 30}
      :teaspoon -> {:milliliter, volume * 5}
      :tablespoon -> {:milliliter, volume * 15}
      :milliliter -> {:milliliter, volume}
    end
  end

  @doc "Convert the milliliter volume-pair to another unit"
  @spec from_milliliter({:milliliter, number}, volume_unit) :: volume_pair()
  def from_milliliter({:milliliter, volume}, unit) do
    case unit do
      :cup -> {unit, volume / 240}
      :fluid_ounce -> {unit, volume / 30}
      :teaspoon -> {unit, volume / 5}
      :tablespoon -> {unit, volume / 15}
      :milliliter -> {unit, volume}
    end
  end

  @doc "Convert from any unit to any unit"
  @spec convert(volume_pair(), volume_unit()) :: volume_pair()
  def convert(volume_pair, to_unit),
    do:
      volume_pair
      |> to_milliliter()
      |> from_milliliter(to_unit)
end
