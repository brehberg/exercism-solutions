defmodule RotationalCipher do
  @moduledoc false
  import Enum, only: [at: 2, map: 2]

  @doc """
  Given a plaintext and amount to shift by, return a rotated string.

  Example:
  iex> RotationalCipher.rotate("Attack at dawn", 13)
  "Nggnpx ng qnja"
  """
  @spec rotate(text :: String.t(), shift :: integer) :: String.t()
  def rotate(text, shift),
    do: text |> String.to_charlist() |> map(&rotate_char(&1, shift)) |> to_string()

  def rotate_char(c, n) when c in ?a..?z, do: ?a..?z |> at(rem(c + n - ?a, 26))
  def rotate_char(c, n) when c in ?A..?Z, do: ?A..?Z |> at(rem(c + n - ?A, 26))
  def rotate_char(c, _), do: c
end
