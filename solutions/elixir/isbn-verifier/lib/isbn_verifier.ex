defmodule IsbnVerifier do
  @moduledoc false
  @value_capture ~r/(\d)(\d)(\d)(\d)(\d)(\d)(\d)(\d)(\d)(\d+)/

  @doc """
    Checks if a string is a valid ISBN-10 identifier

    ## Examples

      iex> IsbnVerifier.isbn?("3-598-21507-X")
      true

      iex> IsbnVerifier.isbn?("3-598-2K507-0")
      false

  """
  @spec isbn?(String.t()) :: boolean
  def isbn?(isbn),
    do: Regex.scan(@value_capture, clean(isbn), capture: :all_but_first) |> valid?()

  @doc false
  @spec clean(String.t()) :: String.t()
  defp clean(input), do: input |> String.replace(~r/X$/, "10") |> String.replace("-", "")

  @doc false
  @spec valid?([[Sting.t()]]) :: boolean
  defp valid?([]), do: false
  defp valid?([values]) when length(values) != 10, do: false
  defp valid?([values]), do: Enum.map(values, &String.to_integer/1) |> compute() == 0

  @doc false
  @spec compute([integer], integer, integer) :: integer
  defp compute(digits, multiple \\ 10, accumulator \\ 0)
  defp compute([], _, result), do: rem(result, 11)
  defp compute([last], _n, acc), do: compute([], 0, acc + last)
  defp compute([next | rest], n, acc), do: compute(rest, n - 1, acc + next * n)
end
