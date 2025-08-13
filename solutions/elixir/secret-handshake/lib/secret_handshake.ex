defmodule SecretHandshake do
  use Bitwise

  @doc """
  Determine the actions of a secret handshake based on the binary
  representation of the given `code`.

  If the following bits are set, include the corresponding action in your list
  of commands, in order from lowest to highest.

  1 = wink
  10 = double blink
  100 = close your eyes
  1000 = jump

  10000 = Reverse the order of the operations in the secret handshake
  """
  @spec commands(code :: integer) :: list(String.t())
  def commands(code), do: secret([], code)

  defp secret(handshake, code) when band(code, 0b00001) == 0b00001,
    do: secret(["wink" | handshake], bxor(code, 0b00001))

  defp secret(handshake, code) when band(code, 0b00010) == 0b00010,
    do: secret(["double blink" | handshake], bxor(code, 0b00010))

  defp secret(handshake, code) when band(code, 0b00100) == 0b00100,
    do: secret(["close your eyes" | handshake], bxor(code, 0b00100))

  defp secret(handshake, code) when band(code, 0b01000) == 0b01000,
    do: secret(["jump" | handshake], bxor(code, 0b01000))

  defp secret(handshake, code) when band(code, 0b10000) == 0b10000, do: handshake
  defp secret(handshake, code) when band(code, 0b00000) == 0b00000, do: Enum.reverse(handshake)
end
