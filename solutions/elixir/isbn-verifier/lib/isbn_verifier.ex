defmodule IsbnVerifier do
  @moduledoc false
  @isbn10_regex ~r/^(\d-?){9}(\d|X)$/

  @doc """
    Checks if a string is a valid ISBN-10 identifier

    ## Examples

      iex> IsbnVerifier.isbn?("3-598-21507-X")
      true

      iex> IsbnVerifier.isbn?("3-598-2K507-0")
      false

  """
  @spec isbn?(String.t()) :: boolean
  def isbn?(isbn) do
    case isbn =~ @isbn10_regex do
      false -> false
      _ -> valid?(isbn)
    end
  end

  @doc false
  @spec valid?(Sting.t()) :: boolean
  defp valid?(values) do
    values
    |> String.replace("-", "")
    |> String.graphemes()
    |> Enum.zip(10..1)
    |> Enum.reduce(0, &compute/2)
    |> rem(11) == 0
  end

  @doc false
  @spec compute({String.t(), integer}, integer) :: integer
  defp compute({"X", 1}, sum), do: 10 + sum
  defp compute({s, n}, sum), do: String.to_integer(s) * n + sum
end
