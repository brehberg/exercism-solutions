defmodule SecretHandshake do
  import Bitwise

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
  def commands(code) do
    []
    |> secret(band(code, 0b01000) == 0b01000, "jump")
    |> secret(band(code, 0b00100) == 0b00100, "close your eyes")
    |> secret(band(code, 0b00010) == 0b00010, "double blink")
    |> secret(band(code, 0b00001) == 0b00001, "wink")
    |> secret(band(code, 0b10000) == 0b10000, &Enum.reverse/1)
  end

  defp secret(handshake, do?, todo)
  defp secret(handshake, false, _), do: handshake
  defp secret(handshake, true, str) when is_binary(str), do: [str, handshake]
  defp secret(handshake, true, todo), do: fun.(handshake)
end
