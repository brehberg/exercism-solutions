defmodule Transpose do
  @doc """
  Given an input text, output it transposed.

  Rows become columns and columns become rows. See https://en.wikipedia.org/wiki/Transpose.

  If the input has rows of different lengths, this is to be solved as follows:
    * Pad to the left with spaces.
    * Don't pad to the right.

  ## Examples

    iex> Transpose.transpose("ABC\\nDE")
    "AD\\nBE\\nC"

    iex> Transpose.transpose("AB\\nDEF")
    "AD\\nBE\\n F"
  """
  @spec transpose(String.t()) :: String.t()
  def transpose(input) do
    {max, lines} = parse(input)

    # all lines are padded to max length with a non-printable character
    # so that Enum.zip_with/2 can be used to transpose the entire matrix
    lines
    |> Enum.map(&String.pad_trailing(&1, max, "\a"))
    |> Enum.map(&String.graphemes/1)
    |> Enum.zip_with(& &1)
    |> Enum.map_join("\n", &String.trim_trailing(Enum.join(&1), "\a"))
  end

  # return tuple with the length of the longest line in the given input
  # and a list of lines each padded with spaces if next line is longer
  @spec parse(String.t()) :: {integer, [String.t()]}
  defp parse(input) do
    lines =
      String.split(input, "\n")
      |> Enum.reverse()
      |> Enum.scan({0, ""}, &add_spaces/2)
      |> Enum.reverse()

    {List.first(lines) |> elem(0), Enum.unzip(lines) |> elem(1)}
  end

  # pad current line with spaces up to the length of the previous line
  @spec add_spaces(String.t(), {integer, String.t()}) :: {integer, String.t()}
  defp add_spaces(line, {n, _}) do
    padded = String.pad_trailing(line, n)
    {String.length(padded), padded}
  end
end
