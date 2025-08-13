defmodule ArmstrongNumber do
  @moduledoc """
  Provides a way to validate whether or not a number is an Armstrong number
  """
  alias Integer, as: I

  @doc """
  An Armstrong number is a number that is the sum of its own digits
  each raised to the power of the number of digits.
  """
  @spec valid?(integer) :: boolean
  def valid?(number), do: number |> I.digits() |> compute() == number

  @doc false
  @spec compute([integer]) :: integer
  defp compute(digits), do: digits |> Enum.map(&I.pow(&1, length(digits))) |> Enum.sum()
end
