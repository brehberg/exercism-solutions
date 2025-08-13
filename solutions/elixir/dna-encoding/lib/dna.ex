defmodule DNA do
  @doc """
  Encode nucleic acid to binary value
  """
  def encode_nucleotide(code_point) do
    case code_point do
      ?\s -> 0b0000
      ?A -> 0b0001
      ?C -> 0b0010
      ?G -> 0b0100
      ?T -> 0b1000
    end
  end

  @doc """
  Decode the binary value to the nucleic acid
  """
  def decode_nucleotide(encoded_code) do
    case encoded_code do
      0b0001 -> ?A
      0b0010 -> ?C
      0b0100 -> ?G
      0b1000 -> ?T
      0b0000 -> ?\s
    end
  end

  @doc """
  Encode a DNA charlist
  """
  def encode(dna), do: do_encode(dna, <<>>)

  @doc false
  defp do_encode([], encoded), do: encoded

  defp do_encode([head | rest], code),
    do: do_encode(rest, <<code::bitstring, encode_nucleotide(head)::4>>)

  @doc """
  Decode a DNA bitstring
  """
  def decode(dna) do
    do_decode(dna, [])
  end

  @doc false
  defp do_decode(<<>>, decoded), do: decoded

  defp do_decode(<<code::4, rest::bitstring>>, text),
    do: do_decode(rest, text ++ [decode_nucleotide(code)])
end
