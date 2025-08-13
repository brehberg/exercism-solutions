defmodule Acronym do
  @moduledoc false

  @doc """
  Generate an acronym from a string.
  "This is a string" => "TIAS"
  """
  @spec abbreviate(String.t()) :: String.t()
  def abbreviate(string),
    do:
      string
      |> String.replace("_", "")
      |> String.split(~r{[\s|-]})
      |> Enum.map_join(&String.first/1)
      |> String.upcase()
end
