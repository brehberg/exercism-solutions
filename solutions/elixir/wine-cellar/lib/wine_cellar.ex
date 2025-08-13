defmodule WineCellar do
  @type wine_color :: :white | :red | :rose

  @doc """
  Explain wine colors

  On the welcome screen of your app, you want to display a short explanation of each wine color.
  """
  @type color_description :: {wine_color, String.t()}
  @spec explain_colors :: list(color_description)
  def explain_colors() do
    [
      white: "Fermented without skin contact.",
      red: "Fermented with skin contact using dark-colored grapes.",
      rose: "Fermented with some skin contact, but not enough to qualify as a red wine."
    ]
  end

  @doc """
  Get all wines of a given color, bottled in a given year, and bottled in a given country

  A bottle of wine is represented as a 3-tuple of grape variety, year, and country of origin.
  The wines are stored by wine color in a keyword list.
  Take a keyword list of wines, a color atom and a keyword list of options.
  The function should return a list of matching wines of a given color as well as
  when given both the :year and the :country option, in any order.
  """
  @type wine :: {grape :: String.t(), year :: integer, country :: String.t()}
  @type(options :: :year | :country | :year, :country | :country, :year)
  @spec filter(keyword(wine), wine_color, keyword(options)) :: list(wine)
  def filter(cellar, color, options \\ [])

  def filter(cellar, color, []),
    do: cellar |> Keyword.get_values(color)

  def filter(cellar, color, year: option),
    do: cellar |> filter(color) |> filter_by_year(option)

  def filter(cellar, color, country: option),
    do: cellar |> filter(color) |> filter_by_country(option)

  def filter(cellar, color, year: option1, country: option2),
    do: cellar |> filter(color, year: option1) |> filter_by_country(option2)

  def filter(cellar, color, country: option1, year: option2),
    do: cellar |> filter(color, country: option1) |> filter_by_year(option2)

  # The functions below do not need to be modified.

  defp filter_by_year(wines, year)
  defp filter_by_year([], _year), do: []

  defp filter_by_year([{_, year, _} = wine | tail], year) do
    [wine | filter_by_year(tail, year)]
  end

  defp filter_by_year([{_, _, _} | tail], year) do
    filter_by_year(tail, year)
  end

  defp filter_by_country(wines, country)
  defp filter_by_country([], _country), do: []

  defp filter_by_country([{_, _, country} = wine | tail], country) do
    [wine | filter_by_country(tail, country)]
  end

  defp filter_by_country([{_, _, _} | tail], country) do
    filter_by_country(tail, country)
  end
end
