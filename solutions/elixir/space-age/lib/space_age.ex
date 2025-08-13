defmodule SpaceAge do
  @type planet ::
          :mercury
          | :venus
          | :earth
          | :mars
          | :jupiter
          | :saturn
          | :uranus
          | :neptune
  @doc false
  defp years_on_earth(seconds), do: seconds / 31557600
  @doc """
  Return the number of years a person that has lived for 'seconds' seconds is
  aged on 'planet', or an error if 'planet' is not a planet.
  """
  @spec age_on(planet, pos_integer) :: {:ok, float} | {:error, String.t()}
  def age_on(:earth, seconds), do: {:ok, years_on_earth(seconds)}
  def age_on(:mercury, seconds), do: {:ok, years_on_earth(seconds) / 0.2408467}
  def age_on(:venus, seconds), do: {:ok, years_on_earth(seconds) / 0.61519726}
  def age_on(:mars, seconds), do: {:ok, years_on_earth(seconds) / 1.8808158}
  def age_on(:jupiter, seconds), do: {:ok, years_on_earth(seconds) / 11.862615}
  def age_on(:saturn, seconds), do: {:ok, years_on_earth(seconds) / 29.447498}
  def age_on(:uranus, seconds), do: {:ok, years_on_earth(seconds) / 84.016846}
  def age_on(:neptune, seconds), do: {:ok, years_on_earth(seconds) / 164.79132}
  def age_on(_,_), do: {:error, "not a planet"}
end
