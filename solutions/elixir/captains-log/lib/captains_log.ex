defmodule CaptainsLog do
  @moduledoc """
  Random generators for data commonly appearing in the captain's log.
  """
  import Enum
  @planetary_classes ["D", "H", "J", "K", "L", "M", "N", "R", "T", "Y"]

  @doc """
  Generate a random planet
  """
  @spec random_planet_class :: String.t()
  def random_planet_class(), do: random(@planetary_classes)

  @doc """
  Generate a random starship registry number
  """
  @spec random_ship_registry_number :: String.t()
  def random_ship_registry_number(), do: "NCC-#{random(to_list(1000..9999))}"

  @doc """
   Generate a random stardate
  """
  @spec random_stardate :: float
  def random_stardate(), do: random_float(41000.0, 42000.0)

  @doc false
  @spec random_float(float, float) :: float
  defp random_float(min, max), do: min + (max - min) * :rand.uniform()

  @doc """
  Format the stardate
  """
  @spec format_stardate(float) :: String.t()
  def format_stardate(stardate), do: to_string(:io_lib.format("~.1f", [stardate]))
end
