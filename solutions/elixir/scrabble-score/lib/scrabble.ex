defmodule Scrabble do
  @moduledoc false
  @letter_values %{
    1 => ~w(A E I O U L N R S T),
    2 => ~w(D G),
    3 => ~w(B C M P),
    4 => ~w(F H V W Y),
    5 => ~w(K),
    8 => ~w(J X),
    10 => ~w(Q Z)
  }

  @doc """
  Calculate the scrabble score for the word.
  """
  @spec score(String.t()) :: non_neg_integer
  def score(word) do
    letter_map = transform(@letter_values)

    word
    |> String.trim()
    |> String.upcase()
    |> String.split("", trim: true)
    |> Enum.reduce(0, &(&2 + letter_map[&1]))
  end

  @doc false
  @spec transform(%{integer => [String.t()]}) :: %{String.t() => integer}
  defp transform(input) do
    # Transforms given Scrabble score format to a new one.
    for {value, letters} <- input,
        letter <- letters,
        into: %{} do
      {letter, value}
    end
  end
end
