defmodule Atbash do
  @moduledoc false
  import List, only: [keyfind: 3]
  @cipher Enum.zip(?a..?z, ?z..?a)
  @to_cipher [from: 0, to: 1]
  @from_cipher [from: 1, to: 0]
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
    |> Enum.map(&translate(&1, @to_cipher))
    |> Enum.chunk_every(@group_size)
    |> Enum.join(" ")
  end

  @doc """
  Decode a given ciphertext to the corresponding plaintext
  """
  @spec decode(String.t()) :: String.t()
  def decode(ciphertext) do
    prepare(ciphertext) |> Enum.map_join(&translate(&1, @from_cipher))
  end

  @doc false
  @spec translate(char, keyword) :: [char]
  defp translate(n, _) when n in ?1..?9, do: [n]
  defp translate(c, from: a, to: b), do: [@cipher |> keyfind(c, a) |> elem(b)]

  @doc false
  @spec prepare(String.t()) :: String.t()
  defp prepare(input) do
    input
    |> String.replace(~r/\W/, "")
    |> String.downcase()
    |> String.to_charlist()
  end
end
