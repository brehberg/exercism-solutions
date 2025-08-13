defmodule Yacht do
  @type category ::
          :ones
          | :twos
          | :threes
          | :fours
          | :fives
          | :sixes
          | :full_house
          | :four_of_a_kind
          | :little_straight
          | :big_straight
          | :choice
          | :yacht

  @doc """
  Calculate the score of 5 dice using the given category's scoring method.
  """
  @spec score(category :: category(), dice :: [integer]) :: integer
  def score(:ones, dice), do: dice |> sum_for_value(1)
  def score(:twos, dice), do: dice |> sum_for_value(2)
  def score(:threes, dice), do: dice |> sum_for_value(3)
  def score(:fours, dice), do: dice |> sum_for_value(4)
  def score(:fives, dice), do: dice |> sum_for_value(5)
  def score(:sixes, dice), do: dice |> sum_for_value(6)

  def score(:little_straight, dice), do: if(dice |> have_range(1..5), do: 30, else: 0)
  def score(:big_straight, dice), do: if(dice |> have_range(2..6), do: 30, else: 0)
  def score(:choice, dice), do: dice |> Enum.sum()
  def score(:yacht, [d, d, d, d, d]), do: 50
  def score(:yacht, _dice), do: 0

  def score(:full_house, dice) do
    case Enum.sort(dice) do
      [s, s, b, b, b] -> if s == b, do: 0, else: dice |> Enum.sum()
      [s, s, s, b, b] -> if s == b, do: 0, else: dice |> Enum.sum()
      _ -> 0
    end
  end

  def score(:four_of_a_kind, dice) do
    case Enum.sort(dice) do
      [s, s, s, s, _] -> s * 4
      [_, b, b, b, b] -> b * 4
      _ -> 0
    end
  end

  @doc false
  @spec sum_for_value([integer], integer) :: integer
  defp sum_for_value(dice, n), do: dice |> Enum.filter(&(&1 == n)) |> Enum.sum()
  @spec have_range([integer], Range.t()) :: boolean
  defp have_range(dice, range), do: Enum.sort(dice) == Enum.to_list(range)
end
