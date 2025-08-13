defmodule DNA do
  @doc """
  Encode nucleic acid to binary value
  """
  @spec encode_nucleotide(char) :: pos_integer
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
  @spec decode_nucleotide(pos_integer) :: char
  def decode_nucleotide(encoded_code) do
    case encoded_code do
      0b0000 -> ?\s
      0b0001 -> ?A
      0b0010 -> ?C
      0b0100 -> ?G
      0b1000 -> ?T
    end
  end

  @doc """
  Encode a DNA charlist
  """
  @spec encode(dna :: charlist) :: bitstring
  def encode(dna), do: do_encode(dna, <<>>)

  @doc false
  @spec do_encode(input :: charlist, result :: bitstring) :: bitstring
  defp do_encode([], encoded), do: encoded

  defp do_encode([char | rest], code),
    do: do_encode(rest, <<code::bitstring, encode_nucleotide(char)::4>>)

  @doc """
  Decode a DNA bitstring
  """
  @spec decode(dna :: bitstring) :: charlist
  def decode(dna), do: do_decode(dna, [])

  @doc false
  @spec do_decode(input :: bitstring, result :: charlist) :: charlist
  defp do_decode(<<>>, decoded), do: decoded

  defp do_decode(<<code::4, rest::bitstring>>, text),
    do: do_decode(rest, text ++ [decode_nucleotide(code)])
end
