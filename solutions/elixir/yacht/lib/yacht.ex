defmodule Yacht do
  @moduledoc false
  import Enum, only: [filter: 2, sort: 1, sum: 1, to_list: 1]

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

  # a straight is worth 30 points if all dice values required are present
  def score(:little_straight, dice), do: dice |> score_for(1..5)
  def score(:big_straight, dice), do: dice |> score_for(2..6)
  def score(:choice, dice), do: sum(dice)
  def score(:yacht, [d, d, d, d, d]), do: 50
  def score(:yacht, _dice), do: 0

  def score(:full_house, dice) do
    case sort(dice) do
      [d, d, d, d, d] -> 0
      [s, s, b, b, b] -> sum(dice)
      [s, s, s, b, b] -> sum(dice)
      _ -> 0
    end
  end

  def score(:four_of_a_kind, dice) do
    case sort(dice) do
      [s, s, s, s, _] -> s * 4
      [_, b, b, b, b] -> b * 4
      _ -> 0
    end
  end

  @doc false
  @spec sum_for_value([integer], integer) :: integer
  defp sum_for_value(dice, num),
    do: dice |> filter(&(&1 == num)) |> sum()

  @doc false
  @spec score_for([integer], Range.t()) :: integer
  defp score_for(dice, nums),
    do: if(sort(dice) == to_list(nums), do: 30, else: 0)
end
