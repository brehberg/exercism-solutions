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
  def valid?(number), do: expand(I.digits(number)) == number

  @doc false
  @spec expand([integer]) :: non_neg_integer
  defp expand(digits), do: Enum.reduce(digits, 0, &(&2 + I.pow(&1, length(digits))))
end
