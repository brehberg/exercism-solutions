defmodule CollatzConjecture do
  @doc """
  calc/1 takes an integer and returns the number of steps required to get the
  number to 1 when following the rules:
    - if number is odd, multiply with 3 and add 1
    - if number is even, divide by 2
  """
  @spec calc(input :: pos_integer()) :: non_neg_integer()
  def calc(input) when not is_integer(input), do: raise(FunctionClauseError)
  def calc(input) when input <= 0, do: raise(FunctionClauseError)
  def calc(input), do: do_calc(input, 0)

  def do_calc(1, step), do: step
  def do_calc(i, step) when rem(i, 2) != 0, do: do_calc(3 * i + 1, step + 1)
  def do_calc(i, step), do: do_calc(div(i, 2), step + 1)
end
