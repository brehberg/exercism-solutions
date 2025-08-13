defmodule RunLengthEncoder do
  @moduledoc false
  alias String, as: Str

  @doc """
  Generates a string where consecutive elements are represented as a data value and count.
  "AABBBCCCC" => "2A3B4C"
  For this example, assume all input are strings, that are all letters or whitespace.
  """
  @spec encode(String.t()) :: String.t()
  def encode(string) do
    Str.to_charlist(string)
    |> Enum.chunk_by(& &1)
    |> Enum.map_join(&encode_chunk/1)
  end

  @doc false
  @spec encode_chunk(charlist) :: String.t()
  defp encode_chunk([char]), do: "#{[char]}"
  defp encode_chunk([char | rest]), do: "#{length(rest) + 1}#{[char]}"

  @doc """
  It should also be able to reconstruct the data into its original form.
  "2A3B4C" => "AABBBCCCC"
  """
  @spec decode(String.t()) :: String.t()
  def decode(string) do
    Regex.scan(~r/(\d*.)/, string, capture: :all_but_first)
    |> Enum.map_join(&decode_capture/1)
  end

  @doc false
  @spec decode_capture([String.t()]) :: String.t()
  defp decode_capture([group]) when byte_size(group) == 1, do: group

  defp decode_capture([group]) do
      count = group |> Str.slice(0..-2//1) |> Str.to_integer()
      group |> Str.last() |> Str.duplicate(count)
  end
end
