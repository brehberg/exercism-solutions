defmodule VariableLengthQuantity do
  @moduledoc """
  VLQ is essentially a base-128 representation of an unsigned integer
  with the addition of the eighth bit to mark continuation of bytes.
  """
  @base 128

  @doc """
  Encode integers into a bitstring of VLQ encoded bytes
  """
  @spec encode(integers :: [integer]) :: binary
  def encode([value]), do: Integer.digits(value, @base) |> do_encode(<<>>)
  def encode(values), do: Enum.map_join(values, &encode([&1]))

  @spec do_encode([integer], binary) :: binary
  defp do_encode([last], encoded), do: encoded <> <<last>>

  defp do_encode([first | rest], encoded),
    do: do_encode(rest, encoded <> <<first + @base>>)

  @doc """
  Decode a bitstring of VLQ encoded bytes into a series of integers
  """
  @spec decode(bytes :: binary) :: {:ok, [integer]} | {:error, String.t()}
  def decode(bytes) do
    case do_decode(bytes, []) do
      [] -> {:error, "incomplete sequence"}
      values -> {:ok, values}
    end
  end

  @spec do_decode(binary, [integer]) :: [integer]
  defp do_decode(<<byte::unsigned, rest::binary>>, digits) when byte < @base,
    do: [combine([byte | digits]) | do_decode(rest, [])]

  defp do_decode(<<byte::unsigned, rest::binary>>, digits),
    do: do_decode(rest, [byte - @base | digits])

  defp do_decode(_, _), do: []

  @spec combine([integer]) :: integer
  defp combine(digits), do: Enum.reverse(digits) |> Integer.undigits(@base)
end
