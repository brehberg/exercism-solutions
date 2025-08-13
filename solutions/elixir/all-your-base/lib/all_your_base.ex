defmodule AllYourBase do
  alias Integer, as: I

  @doc """
  Given a number in input base, represented as a sequence of digits, converts it to output base,
  or returns an error tuple if either of the bases are less than 2
  """

  @spec convert(list, integer, integer) :: {:ok, list} | {:error, String.t()}
  def convert(_, _, output_base) when output_base < 2, do: {:error, "output base must be >= 2"}
  def convert(_, input_base, _) when input_base < 2, do: {:error, "input base must be >= 2"}

  def convert(digits, input_base, output_base) do
    if valid_digits?(digits, input_base) do
      {:ok,
       digits
       |> input_to_integer(input_base, length(digits) - 1, 0)
       |> integer_to_output(output_base, [])}
    else
      {:error, "all digits must be >= 0 and < input base"}
    end
  end

  # convert sequence of digits in input base to whole integer value
  @spec input_to_integer(
          digits :: [non_neg_integer],
          base :: pos_integer,
          position :: non_neg_integer,
          result :: non_neg_integer
        ) :: non_neg_integer
  defp input_to_integer([], _, _, result), do: result

  defp input_to_integer([digit | rest], base, position, result),
    do: input_to_integer(rest, base, position - 1, result + digit * I.pow(base, position))

  # "convert whole integer value to sequence of digits in output base"
  @spec integer_to_output(
          value :: non_neg_integer,
          base :: pos_integer,
          result :: [non_neg_integer]
        ) :: [non_neg_integer]
  defp integer_to_output(value, base, result) when value < base, do: [value | result]

  defp integer_to_output(value, base, result),
    do: integer_to_output(div(value, base), base, [rem(value, base) | result])

  # "check all digits are non-negative and less than input base"
  @spec valid_digits?(digits :: [integer], base :: pos_integer) :: boolean
  defp valid_digits?(digits, base),
    do: digits |> Enum.reduce(true, &(&2 and &1 >= 0 and &1 < base))
end
