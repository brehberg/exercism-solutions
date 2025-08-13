defmodule PigLatin do
  @moduledoc false
  @suffix "ay"
  @unique "qu"
  @vowels ["a", "e", "i", "o", "u"]
  @maybe ["x", "y"]

  @doc """
  Given a `phrase`, translate it a word at a time to Pig Latin.
  """
  @spec translate(phrase :: String.t()) :: String.t()
  def translate(phrase),
    do: phrase |> String.split() |> Enum.map_join(" ", &translate_word/1)

  # If a word begins with a vowel sound, add an "ay" sound to the end of the word
  @spec translate_word(word :: String.t()) :: String.t()
  def translate_word(<<first::binary-size(1), rest::binary>>) when first in @vowels,
    do: first <> rest <> @suffix

  # If a word begins with either "x" or "y" followed by a vowel sound,
  # add an "ay" sound to the end of the word
  def translate_word(<<first::binary-size(1), next::binary-size(1), rest::binary>>)
      when first in @maybe and next not in @vowels,
      do: first <> next <> rest <> @suffix

  # If a word starts with a "qu", move it to the end of the word.
  def translate_word(<<@unique, rest::binary>> = phrase),
    do: translate_word(rest <> @unique)

  # If a word begins with a consonant sound, move it to the end of the word
  def translate_word(<<first::binary-size(1), rest::binary>> = phrase),
    do: translate_word(rest <> first)
end
