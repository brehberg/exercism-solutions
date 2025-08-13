defmodule Darts do
  import Float
  @type position :: {number, number}

  @doc """
  Calculate the score of a single dart hitting a target
  """
  @spec score(position) :: integer
  def score({x, y}), do: return_score(distance(x, y))

  @doc false
  @spec return_score(float) :: integer
  defp return_score(dist) when dist <= 1, do: 10
  defp return_score(dist) when dist <= 5, do: 5
  defp return_score(dist) when dist <= 10, do: 1
  defp return_score(_), do: 0

  @doc false
  @spec distance(number, number) :: float
  defp distance(x, y), do: pow(pow(x / 1, 2) + pow(y / 1, 2), 0.5)
end
