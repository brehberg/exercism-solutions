defmodule CaptainsLog do
  @moduledoc "Random generators for data commonly appearing in the captain's log."
  import Enum, only: [random: 1, to_list: 1]
  @planetary_classes ["D", "H", "J", "K", "L", "M", "N", "R", "T", "Y"]
  @starship_min 1000
  @starship_max 9999
  @stardate_min 41000.0
  @stardate_max 42000.0

  @doc "Generate a random planet"
  @spec random_planet_class :: String.t()
  def random_planet_class(), do: random(@planetary_classes)

  @doc "Generate a random starship registry number"
  @spec random_ship_registry_number :: String.t()
  def random_ship_registry_number(),
    do: "NCC-#{random(to_list(@starship_min..@starship_max))}"

  @doc "Generate a random stardate"
  @spec random_stardate :: float
  def random_stardate(), do: random_float(@stardate_min, @stardate_max)

  @doc false
  @spec random_float(float, float) :: float
  defp random_float(min, max), do: min + (max - min) * :rand.uniform()

  @doc "Format the stardate"
  @spec format_stardate(float) :: String.t()
  def format_stardate(stardate), do: to_string(:io_lib.format("~.1f", [stardate]))
end
