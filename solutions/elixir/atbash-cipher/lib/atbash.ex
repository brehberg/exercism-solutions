defmodule Atbash do
  @moduledoc false
  import List, only: [keyfind: 3]
  @plain_cipher Enum.zip(?a..?z, ?z..?a)
  @group_size 5

  @doc """
  Encode a given plaintext to the corresponding ciphertext

  ## Examples

  iex> Atbash.encode("completely insecure")
  "xlnko vgvob rmhvx fiv"
  """
  @spec encode(String.t()) :: String.t()
  def encode(plaintext) do
    prepare(plaintext)
    |> Enum.map(&translate(&1, 0, 1))
    |> Enum.chunk_every(@group_size)
    |> Enum.join(" ")
  end

  @doc """
  Decode a given ciphertext to the corresponding plaintext
  """
  @spec decode(String.t()) :: String.t()
  def decode(ciphertext) do
    prepare(ciphertext) |> Enum.map_join(&translate(&1, 1, 0))
  end

  @doc false
  @spec translate(char, from_position :: integer, to_position :: integer) :: [char]
  defp translate(n, _, _) when n in ?1..?9, do: [n]
  defp translate(c, from, to), do: [@plain_cipher |> keyfind(c, from) |> elem(to)]

  @doc false
  @spec prepare(String.t()) :: String.t()
  defp prepare(input) do
    input
    |> String.replace(~r/\W/, "")
    |> String.downcase()
    |> String.to_charlist()
  end
end
