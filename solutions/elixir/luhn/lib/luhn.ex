defmodule Luhn do
  @doc """
  Checks if the given number is valid via the luhn formula
  """
  @spec valid?(String.t()) :: boolean
  def valid?(number) do
    trimmed = number |> String.replace(~s/ /, "")

    trimmed =~ ~r/^\d{2,}$/ and
      [0 | get_digits(trimmed)]
      |> Enum.map_every(2, &double/1)
      |> Enum.sum()
      |> rem(10) == 0
  end

  @spec get_digits(String.t()) :: [integer]
  defp get_digits(number) do
    number
    |> String.graphemes()
    |> Enum.map(&String.to_integer/1)
    |> Enum.reverse()
  end

  @spec double(integer) :: integer
  defp double(n) when n < 5, do: n * 2
  defp double(n), do: n * 2 - 9
end
