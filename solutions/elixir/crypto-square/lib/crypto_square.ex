defmodule CryptoSquare do
  @doc """
  Encode string square methods
  ## Examples

    iex> CryptoSquare.encode("abcd")
    "ac bd"
  """
  @spec encode(String.t()) :: String.t()
  def encode(str),
    do: str |> normalize() |> convert()

  @spec normalize(String.t()) :: [String.t()]
  defp normalize(input) do
    input
    |> String.replace(~r/\W/, "")
    |> String.downcase()
    |> String.graphemes()
  end

  @spec convert([String.t()]) :: String.t()
  defp convert([]), do: ""

  defp convert(letters) do
    c = letters |> length() |> :math.sqrt() |> ceil()

    letters
    |> Enum.chunk_every(c, c, Stream.cycle(["\s"]))
    |> Enum.zip_with(& &1)
    |> Enum.map_join("\s", &Enum.join/1)
  end
end
